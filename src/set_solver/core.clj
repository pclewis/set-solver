(ns set-solver.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :refer [abs round]]
            [com.evocomputing.colors :as colors]
            [clojure.math.combinatorics :as combo]
            [set-solver.reusable-buffer :refer [reusable]]
            [set-solver.match-shapes :refer [match-shapes-i1 match-shapes-i2 match-shapes-i3]]
            [set-solver.util :refer :all]
            [set-solver.perspective-ratio :refer [estimate-focal-length rectangle-aspect-ratio ]])

  (:import [java.nio ByteBuffer ByteOrder]
            [org.opencv.core Core CvType Moments Mat MatOfDouble MatOfByte MatOfInt4 MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria Point Scalar Rect MatOfKeyPoint]
            [org.opencv.imgproc Imgproc]
            [org.opencv.imgcodecs Imgcodecs]
            [org.opencv.features2d FeatureDetector Features2d]))


;; card dimensions: 2 1/4" x 3 1/2"

(def frame-rate 15)

(def width 1024)
(def height 768)
(def n-pixels (* width height))
(def card-shape [0.18457629793074265
                 0.00638568929006775
                 1.2985361967142974E-5
                 1.835864996651821E-7
                 2.575287665059767E-13
                 1.3848375876118038E-8
                 1.1843630759414145E-13])

;; STRATEGY THE BEST: just use intensity
;;  it works surprisingly well

;; STRATEGY 1: use saturation, fix shadows
;;  idea: cards are low-saturation, high-lightness
;;        shadows are mid-saturation, mid-lightness
;;  so: negate saturation channel, add to lightness
;;  this is pretty good at removing shadows from card faces,
;;  but not sure that's really a problem that needs solving.
;;  does not seem to really find anything that intensity does not.


;; STRATEGY 3: find shapes, extrapolate card location
;; caveat: hu moments not very reliable without fixing perspective distortion

;; FIXUP 1: Pick 4 points on contour, see if they're the right shape
;; FIXUP 2: Pick 5 points on contour, ABCDE, where angle at B, C, and D is correct.
;;          Extend line BA and DE to meet at F, see if shape BCDF works.
;; FIXUP 3: Pick 4 points ABCD where A and D are on the edge of the image
;;          extend A and D to correct ratio and connet


(defn color-name [hue sat]
  (cond (< 60 hue 180) :green
        (< 25 hue 340) :purple
        (< sat 25)     :purple
        :else          :red))

(defn scalar-color-name [color]
  (let [[h s _] (scalar-to-hsl color)]
    (color-name h s)))

(defn shape-name [contour]
  (condp < (first (contour-hu-invariants contour))
    0.180 :oval
    0.200 :diamond
    :squiggle))

(defn find-sets [cards]
  (filter #(->> (map (juxt :shape :color :fill :count) %)
                transpose
                (map (comp count distinct))
                (every? #{1 3}))
          (combo/combinations cards 3)))

(defn get-perspective-ratio [pts img]
  (let [pt-list (mapcat #(list (.x %) (.y %)) (mat-seq pts))
        center [(/ (.width img) 2.0)
                (/ (.height img) 2.0)]
        result (apply rectangle-aspect-ratio (concat pt-list center))]
    ;(println pt-list center result)
    result))

(defn efl [pts img]
  (let [pt-list (mapcat #(list (.x %) (.y %)) (mat-seq pts))
        center [(/ (.width img) 2.0)
                (/ (.height img) 2.0)]
        result (apply estimate-focal-length (concat pt-list center))]
    result))

(defn should-rotate? [pts img]
  (> (get-perspective-ratio pts img) 1.2))

(defn identify-card
  [img contour]
  (let [target (Mat/zeros 600 300 CvType/CV_8UC3)
        target-points (MatOfPoint2f.)
        tright (-> target .cols dec)
        tbottom (-> target .rows dec)
        br (Imgproc/boundingRect contour)
        c2f (MatOfPoint2f.)
        approx2f (MatOfPoint2f.)
        approx (MatOfPoint.)
        _ (.convertTo contour c2f CvType/CV_32FC2)
        _ (Imgproc/approxPolyDP c2f approx2f
                                (* 0.02 (Imgproc/arcLength c2f true))
                                true)
        _ (.convertTo approx2f approx CvType/CV_32S)
        approx2fl (.toList approx2f)
        center (center-point approx2fl)
        a2f-sorted (sort-rectangle-points-angle (.toList approx2f))
        _ (.fromList approx2f a2f-sorted)
        rot (if (should-rotate? a2f-sorted img)
              #(concat (rest %) (take 1 %))
              identity)
        _ (.fromList target-points (rot (map #(Point. %1 %2)
                                             [tright 0 0       tright]
                                             [0      0 tbottom tbottom])))
        trans-mat (Imgproc/getPerspectiveTransform approx2f target-points)
        target-gray (Mat/zeros (.size target) CvType/CV_8U)
        target-edges (Mat. (.size target) CvType/CV_8U)]

    (Imgproc/warpPerspective img target trans-mat (.size target))
    (Imgproc/cvtColor target target-gray Imgproc/COLOR_BGR2GRAY)
    (Imgproc/blur target-gray target-gray (Size. 2 2))
    (let [m (-> (Core/mean target-gray) .val first (* 0.8))]
      (Imgproc/adaptiveThreshold target-gray target-edges 255
                                         Imgproc/ADAPTIVE_THRESH_GAUSSIAN_C
                                         Imgproc/THRESH_BINARY_INV
                                         9 3)
      (Imgproc/medianBlur target-edges target-edges 5)
      (comment
        (Imgproc/threshold target-gray target-edges m 255 Imgproc/THRESH_BINARY_INV))
      (comment
        (let [channels (java.util.LinkedList.)
              hsl (Mat.)
              _ (Imgproc/cvtColor target hsl Imgproc/COLOR_BGR2HSV)
              _ (Core/split hsl channels)
              #_(Imgproc/cvtColor (nth (vec channels) 2) target Imgproc/COLOR_GRAY2BGR)
              [h s l] (vec channels)
              #_(Imgproc/blur l l (Size. 2 2))
              #_(Core/subtract l s s)
              _ (Imgproc/Canny l target-edges 50 100 -1 true)]
          (Imgproc/dilate target-edges target-edges (Mat.))
          ))

      (Imgproc/rectangle target-edges
                         (Point. 0 0)
                         (.br (Rect. (Point. 0 0) (.size target)))
                         (Scalar. 0 0 0) 50))

    (let [card-contours (java.util.LinkedList.)
          card-hierarchy (MatOfInt4.)
          cmean (Core/mean target target-edges)
          [edge-h edge-s edge-l] (scalar-to-hsl cmean)
          cname (scalar-color-name cmean)
          _ (Imgproc/findContours target-edges card-contours card-hierarchy Imgproc/RETR_EXTERNAL Imgproc/CHAIN_APPROX_SIMPLE)
          card-contours (filter #(< 1 (Imgproc/contourArea %)) card-contours)
          sname (shape-name (first card-contours))
          mask (Mat/zeros (.size target) CvType/CV_8U)
          _ (Imgproc/drawContours mask card-contours -1 (Scalar. 255 255 255) -1)
          _ (Imgproc/drawContours mask card-contours -1 (Scalar. 0 0 0) 30)
          inside-color (Core/mean target mask)
          [inside-h inside-s inside-l] (scalar-to-hsl inside-color)
          _ (Imgproc/drawContours mask card-contours -1 (Scalar. 255 255 255) 30)
          _ (Imgproc/drawContours mask card-contours -1 (Scalar. 0 0 0) 10)
          _ (Imgproc/drawContours mask card-contours -1 (Scalar. 0 0 0) -1)
          outside-color (Core/mean target mask)
          [outside-h outside-s outside-l] (scalar-to-hsl outside-color)
          fill (cond (and (close-enough? inside-l edge-l 5)
                          (close-enough? inside-h edge-h 5))
                     :filled
                     (and (close-enough? inside-l outside-l 5)
                          (close-enough? inside-h outside-h 30)
                          (close-enough? inside-s outside-s 5)) :empty
                          :else :shaded)

          #_ (comment
               channels (java.util.LinkedList.)
               hsl (Mat.)
               _ (Imgproc/cvtColor target hsl Imgproc/COLOR_BGR2HSV)
               _ (Core/split hsl channels)
               #_(Imgproc/cvtColor (nth (vec channels) 2) target Imgproc/COLOR_GRAY2BGR)
               [h s l] (vec channels)
               #_(Imgproc/blur l l (Size. 2 2))
               #_(Core/subtract l s s)
               _ (Imgproc/Canny l target-edges 50 100 -1 true)
               mask (Mat/zeros (.size hsl) CvType/CV_8U)
               _ (Imgproc/rectangle mask (Point. 30 30) (Point. 270 570) (Scalar. 255 255 255) 5)
               #_(Core/absdiff h (Scalar. 30 0 0) h )
               _ (Imgproc/cvtColor target-edges target Imgproc/COLOR_GRAY2BGR)
               mean-sat (first (.val (Core/mean (second channels) mask)))
               #_(Imgproc/threshold target target (* 1.2 mean-sat) 255 Imgproc/THRESH_BINARY)
               #_(Imgproc/dilate target target (Mat.))
               _ (Imgproc/dilate target target (Mat.)))

          _(let [mask (Mat/zeros (.size target) CvType/CV_8U)
                 _ (Imgproc/rectangle mask (Point. 30 30) (Point. 270 570) (Scalar. 255 255 255) 5)
                 mm (Core/mean target mask)
                 m (-> (Core/mean target-gray) .val first (* 0.9))
                  te (Mat.)]
             ;(Imgproc/threshold target-gray target-edges 128 255 (bit-or Imgproc/THRESH_BINARY_INV Imgproc/THRESH_OTSU))
             (comment
              (Imgproc/adaptiveThreshold target-gray target-edges 255
                                          Imgproc/ADAPTIVE_THRESH_GAUSSIAN_C
                                          Imgproc/THRESH_BINARY_INV
                                          13 3)
                                        ;(Imgproc/bilateralFilter te target-edges 40 80 80)
                                        ;(Imgproc/erode te target-edges (Mat.))
               (Imgproc/medianBlur target-edges target-edges 11)
               (Imgproc/dilate target-edges target-edges (Mat.)))
             (comment
               (Core/absdiff target mm target))
             (Imgproc/morphologyEx target target Imgproc/MORPH_GRADIENT
                                   (Mat/ones 5 5 CvType/CV_8U))
             (Imgproc/cvtColor target target-gray Imgproc/COLOR_BGR2GRAY)
             (comment
               (Imgproc/adaptiveThreshold target-gray target-edges 255
                                          Imgproc/ADAPTIVE_THRESH_GAUSSIAN_C
                                          Imgproc/THRESH_BINARY_INV
                                          29 4))
             (Imgproc/threshold target-gray target-edges 0 255 (bit-or Imgproc/THRESH_BINARY Imgproc/THRESH_OTSU))
             ;(Imgproc/dilate target-edges target-edges (Mat.))

             (Imgproc/rectangle target-edges
                                (Point. 0 0)
                                (.br (Rect. (Point. 0 0) (.size target)))
                                (Scalar. 0 0 0) 50)
             (Imgproc/cvtColor target-edges target Imgproc/COLOR_GRAY2BGR))
          result {:count (count card-contours)
                  :shape sname
                  :color cname
                  :fill fill
                  :contour contour
                  :bb br
                  :image target}
          ]
      (if 1
        (into result {:debug {:inside-color [inside-h inside-s inside-l
                                             (colors/rgb-hexstr (colors/create-color {:h inside-h :s inside-s :l inside-l}))]
                              :outside-color [outside-h outside-s outside-l
                                              (colors/rgb-hexstr (colors/create-color {:h outside-h :s outside-s :l outside-l}))]
                              :contour-color [edge-h edge-s edge-l
                                              (colors/rgb-hexstr (colors/create-color {:h edge-h :s edge-s :l edge-l}))]}})
        result))))

(defn distinct-contours
  "Remove contours that are very similar"
  [contours]
  (reduce (fn [cs c]
            (if (some #(rect-close-enough? (Imgproc/boundingRect %)
                                           (Imgproc/boundingRect c))
                      cs)
              cs
              (conj cs c)))
          []
          contours))

(defn min-area-rect
  [contour]
  (let [c2f (MatOfPoint2f.)]
    (.convertTo contour c2f CvType/CV_32FC2)
    (Imgproc/minAreaRect c2f)))

(defn remove-contained-rects
  [contours]
  (let [rects (map #(vector % (Imgproc/boundingRect %)) contours)]
    (remove (fn [c] (let [r (min-area-rect c)
                         center (.center r)
                         area (* (-> r .size .width)
                                 (-> r .size .height))]
                     (some (fn [[oc or]]
                             (and (not= oc c)
                                  (> (.area or) area)
                                  (.contains or center)))
                           rects)))
            contours)))

(defn- debug-rect [img debug-fn contours]
  (debug-fn
   "rectangles"
   (fn [canvas]
     (doseq [contour contours]
       (let [approx (simplify-shape contour)
             color (case (round (Math/abs (- 4 (.rows approx))))
                     0 (Scalar. 255 0 0)
                     1 (Scalar. 192 0 0)
                     2 (Scalar. 128 0 0)
                     (Scalar. 32 0 0))]
         (Imgproc/drawContours canvas (list approx) 0 color)
         (when (= 4 (.rows approx))
           (let [pts (shuffle-rectangle-points-angle (.toList approx))
                 center (center-point pts)
                 angle1 (apply angle-3p (take 3 pts))
                 angle2 (apply angle-3p (take-last 3 pts))
                 [p0 p1 p2 p3] (vec pts)
                 ratio (get-perspective-ratio pts canvas)
                 ;ratio (if (> ratio 1.2) (- (/ 1 ratio)) ratio)
                 ]
             (doseq [pt pts]
               (Imgproc/putText canvas (format "%.1f" (Math/atan2 (- (.y pt) (.y center))
                                                                  (- (.x center) (.x pt))))
                                pt
                                Core/FONT_HERSHEY_COMPLEX 0.6 (Scalar. 255 0 0) 2))
             (Imgproc/circle canvas p0 6 (Scalar. 255 0 0))
             (Imgproc/circle canvas p1 6 (Scalar. 255 0 0) -1)
             (Imgproc/circle canvas p2 12 (Scalar. 255 0 0))
             (Imgproc/line canvas p0 p2 (Scalar. 255 0 0) 1)
             (Imgproc/line canvas p1 p3 (Scalar. 255 0 0) 1)
             (Imgproc/circle canvas center 6 (Scalar. 255 0 0))
             (let [[v0x v0y] (intersect-lines [(.x p0) (.y p0) (.x p1) (.y p1)]
                                              [(.x p2) (.y p2) (.x p3) (.y p3)])
                   [v1x v1y] (intersect-lines [(.x p1) (.y p1) (.x p2) (.y p2)]
                                              [(.x p3) (.y p3) (.x p0) (.y p0)])]
               (Imgproc/putText canvas (format "V0=(%.0f,%.0f)" v0x v0y)
                                (center-point pts)
                                Core/FONT_HERSHEY_COMPLEX 0.5 (Scalar. 255 0 0))
               (Imgproc/putText canvas (format "V1=(%.0f,%.0f)" v1x v1y)
                                (point-add (center-point pts) (Point. 0 25))
                                Core/FONT_HERSHEY_COMPLEX 0.5 (Scalar. 255 0 0)))
             (when-not (pts-rectangle? pts)
               (Imgproc/putText canvas (format "%.1f" angle1) p2
                                Core/FONT_HERSHEY_COMPLEX 1 (Scalar. 255 0 0)))))))
     ))
  contours)


(defn debug-rectanglify
  [debug-fn combos]
  (debug-fn "rectanglify"
   (fn [canvas]
     (doseq [[i pts] (enumerate combos)
             :let [color (if (pts-rectangle? pts)
                           (Scalar. 255 0 0)
                           (Scalar. 64 0 0))]]
       (doseq [[pt1 pt2] (connected-combinations pts 2)
               :let [ofs (Point. 0 (* i -50))]]
         (Imgproc/line canvas (point-add pt1 ofs) (point-add pt2 ofs)
                       color 1)))))
  combos)

(defn rectanglify
  [contour debug-fn]
  (let [c2f (MatOfPoint2f.)
        approx2f (MatOfPoint2f.)
        approx (MatOfPoint.)
        _ (.convertTo contour c2f CvType/CV_32FC2)
        _ (Imgproc/approxPolyDP c2f approx2f
                                (* 0.02 (Imgproc/arcLength c2f true))
                                true)
        _ (.convertTo approx2f approx CvType/CV_32S)
        ]
    (when (< 4 (.rows approx2f) 8)
      (let [pts (sort-rectangle-points-angle (.toList approx2f))]
        (when-let [new-contour (->> (connected-combinations pts 4)
                                    ;(debug-rectanglify debug-fn)
                                    (filter pts-rectangle?)
                                    (first))]
          (.fromList approx2f new-contour)
          (.convertTo approx2f contour CvType/CV_32S))))
    contour))

(defn- find-cards
  [img debug-fn]
  (let [contours (java.util.LinkedList.)]
    (Imgproc/findContours img contours (MatOfInt4.)
                          Imgproc/RETR_EXTERNAL
                          Imgproc/CHAIN_APPROX_SIMPLE)

   (debug-fn "Contours-pre"
    #(Imgproc/drawContours % contours -1 (Scalar. 255 0 0) 1))

    (let [contours
          (->> contours
               (filter #(< (* (.total img) 0.01)
                           (Math/abs (Imgproc/contourArea %))
                           (* (.total img) 0.5)))
               (filter #(> 500 (.rows %)))
               ;(filter #(> 0.5 (match-shapes-i1 card-shape (contour-hu-invariants %))))
               (map simplify-shape)
               (map #(rectanglify % debug-fn))
               (debug-rect img debug-fn)
               (filter rectangle?)
               (filter #(correct-aspect-ratio? img %))
               ;(filter #(< 1.0 (rect-ratio (.size (min-area-rect %))) 2.5))
               distinct-contours)]
      (debug-fn "Contours-post"
       (fn [canvas]
         (doseq [[i c] (enumerate contours)]
           (Imgproc/drawContours canvas contours i
                                 (Scalar. (rand-int 255) 0 0) 1)
           (Imgproc/putText canvas (str (.rows c))
                            (point-add (first (.toList c)) (Point. 0 50))
                            Core/FONT_HERSHEY_COMPLEX 1 (Scalar. 255 0 0)))))
      contours)))

(defn- intensity
  [rgb-img]
  (let [channels (java.util.LinkedList.)]
    (Core/split rgb-img channels)
    (Core/divide (.get channels 0) (Scalar. 3 0 0) (.get channels 0))
    (Core/divide (.get channels 1) (Scalar. 3 0 0) (.get channels 1))
    (Core/divide (.get channels 2) (Scalar. 3 0 0) (.get channels 2))
    (Core/add (.get channels 0) (.get channels 1) (.get channels 0))
    (Core/add (.get channels 0) (.get channels 2) (.get channels 0))
    (.release (.get channels 1))
    (.release (.get channels 2))
    (.get channels 0)))

(defn find-cards-intensity
  [img debug-fn]
  (let [img-i (intensity img)
        work (Mat. (.size img) CvType/CV_8U)]
    (debug-fn "intensity" (constantly img-i))
    (debug-fn "intensity canny"
              (fn [canvas]
                (Imgproc/Canny (.clone img-i) canvas 40 200)))

    (debug-fn "canny lines"
              (fn [canvas]
                (let [lines (MatOfInt4.)]
                  (Imgproc/Canny (.clone img-i) canvas 40 200)
                  (Imgproc/HoughLinesP canvas lines 1 (/ Math/PI 90) 10 20 15)
                  (.setTo canvas (Scalar. 0 0 0))
                  (doseq [[l i] (map vector (partition 4 (.toList lines)) (range))
                          :let [color (Scalar. (mod (* i 16) 256) 0 0)] ]
                    (Imgproc/line canvas
                                  (Point. (nth l 0) (nth l 1))
                                  (Point. (nth l 2) (nth l 3))
                                  color
                                  2)))))
    (let [result
          (-> (for [thresh (range 64 193 32)]
                (do (Imgproc/threshold img-i work thresh 255 Imgproc/THRESH_BINARY)
                    (find-cards work #(debug-fn (str "thresh-" thresh "-" %1) %2))))
              (doall)
              (flatten)
              (distinct-contours)
              ;(remove-contained-rects)
              )]
      (debug-fn "FINAL Contours" #(Imgproc/drawContours % result -1 (Scalar. 255 0 0) 1))
      result)))

(defn find-cards-experimenting
  [img debug-fn]
  (let [img-hsl (Mat. (.size img) CvType/CV_8UC3)
        channels (java.util.LinkedList.)]
    (Imgproc/cvtColor img img-hsl Imgproc/COLOR_RGB2HSV)
    (Core/split img-hsl channels)
    (let [[h s l] (vec channels)
          not-s (Mat.)
          s-thresh (Mat.)
          i (intensity img)]
      (Core/bitwise_not s not-s)
      (debug-fn "hue" (constantly h))
      (debug-fn "lightness" (constantly l))
      (debug-fn "saturation" (constantly s))
      (debug-fn "inverse saturation" (constantly not-s))
      (debug-fn "intensity" (constantly i))
      (debug-fn (str "add" )
                (fn [canvas]
                  (Core/add not-s l canvas)))
      (debug-fn (str "add-morph" )
                (fn [canvas]
                  (Core/add not-s l canvas)
                  (Imgproc/morphologyEx canvas canvas Imgproc/MORPH_GRADIENT
                                        (Mat/ones 5 5 CvType/CV_8U))
                  (comment
                    (Imgproc/threshold canvas canvas 64 255
                                       (bit-or Imgproc/THRESH_OTSU
                                               Imgproc/THRESH_BINARY)))))

      (debug-fn (str "intensity-morph" )
                (fn [canvas]
                  (Imgproc/morphologyEx i canvas Imgproc/MORPH_GRADIENT
                                        (Mat/ones 5 5 CvType/CV_8U))
                  (comment (Imgproc/adaptiveThreshold canvas canvas 255
                                                      Imgproc/ADAPTIVE_THRESH_MEAN_C
                                                      Imgproc/THRESH_BINARY_INV
                                                      9 3))
                  (Imgproc/threshold canvas canvas 64 255
                                     (bit-or Imgproc/THRESH_OTSU
                                             Imgproc/THRESH_BINARY))
                  ;(Imgproc/threshold canvas canvas 64 255 Imgproc/THRESH_BINARY)
                  ))
      (debug-fn (str "Mul" )
                  (fn [canvas]
                    (Core/multiply not-s l canvas)))
      (doseq [thresh (range 32 255 16)]
        (Imgproc/threshold s s-thresh thresh 255 Imgproc/THRESH_BINARY_INV)
        (comment
          (Imgproc/adaptiveThreshold s s 255
                                     Imgproc/ADAPTIVE_THRESH_MEAN_C
                                     Imgproc/THRESH_BINARY_INV
                                     9 3))

        (debug-fn (str "Saturation Threshold-" thresh)
                  (constantly s-thresh))
        (debug-fn (str "Intensity Threshold-" thresh)
                  (fn [canvas] (Imgproc/threshold i canvas thresh 255 Imgproc/THRESH_BINARY)))
        (debug-fn (str "add-thresh-" thresh )
                (fn [canvas]
                  (Core/add not-s l canvas)
                  (Imgproc/threshold canvas canvas thresh 255 Imgproc/THRESH_BINARY)))))


    )
  [])

(defn find-cards-blob
  "Find cards using blob detection"
  [img debug-fn]
  (let [mask (Mat/zeros (.size img) CvType/CV_8U)
        fd (FeatureDetector/create FeatureDetector/SIMPLEBLOB)
        kp (MatOfKeyPoint.)
        img-hsl (Mat. (.size img) CvType/CV_8UC3)
        channels (java.util.LinkedList.)
        sat-mask (Mat. (.size img) CvType/CV_8U)
        abs-diff (Mat. (.size img) CvType/CV_8U)]
    (.read fd "test.yaml")
    (.detect fd img kp)
    (debug-fn "keypoints" #(Features2d/drawKeypoints img kp % (Scalar. 255 0 0) 1))
    (Imgproc/cvtColor img img-hsl Imgproc/COLOR_RGB2HSV)
    (Core/split img-hsl channels)
    (debug-fn "hue" (constantly (.get channels 0)))
    (debug-fn "saturation" (constantly (.get channels 1)))
    (debug-fn "value" (constantly (.get channels 2)))
    (Imgproc/threshold (.get channels 1) sat-mask 60 255
                       Imgproc/THRESH_BINARY_INV)
    (debug-fn "satmask" (constantly sat-mask))


    ;; FIXME ugly
    (let [result
          (remove-contained-rects
           (distinct-contours
            (flatten
             (doall
              (for [[i p] (enumerate (.toList kp))]
                (do
                  (.setTo mask (Scalar. 0 0 0))
                  (Imgproc/circle mask (.pt p) 30 (Scalar. 255 255 255) 3)
                  (Core/multiply sat-mask mask mask)
                  (debug-fn (str "Blobmask" i) (constantly mask))
                  (let [avg (Core/mean img mask)
                        threshed (Mat. (.size img) CvType/CV_8U)]
                    (Core/absdiff img avg abs-diff)
                    (debug-fn (str "absdiff" i) (constantly abs-diff))
                    (Imgproc/cvtColor abs-diff abs-diff Imgproc/COLOR_BGR2GRAY)
                    (doall
                     (for [thresh (range 30 150 50)]
                       (do
                         (Imgproc/threshold abs-diff threshed thresh 255
                                            Imgproc/THRESH_BINARY_INV)
                         (Imgproc/erode threshed threshed (Mat.))
                         (debug-fn (str "threshed-" thresh "-" i) (constantly threshed))
                         (find-cards threshed #(debug-fn (str %1 "-" thresh "-" i) %2))))))))))))]

      (debug-fn "FINAL Contours"
       #(Imgproc/drawContours % result -1 (Scalar. 255 0 0) 1))

      result)

    (comment ;; all blobs at once
     (doseq [p (.toList kp)]
        (Imgproc/circle mask (.pt p) 30 (Scalar. 255 255 255) 3))
      (Core/multiply sat-mask mask mask)
      (debug-fn "Blob mask" (constantly mask))
      (let [avg (Core/mean img mask)
            canny (Mat.)]
        (Core/absdiff img avg abs-diff)
        (Imgproc/cvtColor abs-diff abs-diff Imgproc/COLOR_BGR2GRAY)
        (Imgproc/threshold abs-diff abs-diff 40 255
                           Imgproc/THRESH_BINARY_INV)
        (debug-fn "abs-diff" (constantly abs-diff))
        (Imgproc/Canny abs-diff canny 40 50)
        (debug-fn "canny" (constantly canny))
        (find-cards canny debug-fn) ))))


(defn find-cards-debug
  "Return set of contours representing cards in img."
  [img debug-fn]
  (let [img-gray (Mat.)
        img-hsl (Mat.)
        img-sl (Mat.)
        img-intensity (Mat.)
        canny-output (Mat.)
        hierarchy (MatOfInt4.)
        contours (java.util.LinkedList.)
        channels (java.util.LinkedList.)]
    (Imgproc/cvtColor img img-gray Imgproc/COLOR_BGR2GRAY)
    (debug-fn "Grayscale image" (constantly img-gray))

    (Imgproc/cvtColor img img-hsl Imgproc/COLOR_RGB2HSV)
    (Core/split img-hsl channels)
    (debug-fn "hue" (constantly (.get channels 0)))
    (debug-fn "saturation" (constantly (.get channels 1)))
    (debug-fn "value" (constantly (.get channels 2)))
    (Core/add (.get channels 1) (.get channels 2) img-sl)
    (debug-fn (constantly img-sl) "plus")

    (Core/split img channels)
    (debug-fn "red" (constantly (.get channels 0)))
    (debug-fn "green" (constantly (.get channels 1)))
    (debug-fn "blue" (constantly (.get channels 2)))

    (Core/divide (.get channels 0) (Scalar. 3 0 0) (.get channels 0))
    (Core/divide (.get channels 1) (Scalar. 3 0 0) (.get channels 1))
    (Core/divide (.get channels 2) (Scalar. 3 0 0) (.get channels 2))
    (Core/add (.get channels 0) (.get channels 1) img-intensity)
    (Core/add img-intensity (.get channels 2) img-intensity)
    (debug-fn "intensity-base" (constantly img-intensity))

   (debug-fn "Intensity-adaptive"
     (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)
                 canvas2 (Mat/zeros (.size img) CvType/CV_8UC3)
                 lines (MatOfInt4.)]

             (comment (Imgproc/Canny canvas canvas 20 50))
             ;(Imgproc/threshold img-intensity canvas 100 255 Imgproc/THRESH_BINARY)
             (Imgproc/adaptiveThreshold img-intensity canvas 255
                                        Imgproc/ADAPTIVE_THRESH_MEAN_C
                                        Imgproc/THRESH_BINARY_INV
                                        9 10)
             (comment
               (Imgproc/adaptiveThreshold canvas canvas 255
                                          Imgproc/ADAPTIVE_THRESH_GAUSSIAN_C
                                          Imgproc/THRESH_BINARY_INV
                                          11 5))
             ;(Imgproc/threshold canvas canvas 64 255 Imgproc/THRESH_BINARY)
             ;(Imgproc/findContours canvas contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_TC89_KCOS (Point. 0 0))
             (comment
               (doseq [[c i] (map vector contours (range))]
                 (Imgproc/drawContours canvas2 (list c) 0 (Scalar. (mod (* i 16) 256) (mod (* i i) 256) (mod (Math/sqrt i) 256)) 1)))
             ;(Imgproc/dilate canvas canvas (Mat.))
             (comment
               (Imgproc/HoughLinesP canvas lines 1 (/ Math/PI 120) 80 30 10)
               (doseq [[l i] (map vector (partition 4 (.toList lines)) (range))
                       :let [color (Scalar. (mod (* i 16) 256)
                                            (mod (* i i) 256)
                                            (mod (Math/sqrt i) 256))] ]
                 (Imgproc/line canvas2
                               (Point. (nth l 0) (nth l 1))
                               (Point. (nth l 2) (nth l 3))
                               color
                               2)))
             canvas)))

   (comment
     (let [fd (FeatureDetector/create FeatureDetector/SIMPLEBLOB)]
       (.write fd "test.yaml"))

     )

   (debug-fn "Blobs"
     (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)
                fd (FeatureDetector/create FeatureDetector/SIMPLEBLOB)
                kp (MatOfKeyPoint.)]
            (.read fd "test.yaml")
            (.detect fd img kp)
            (Features2d/drawKeypoints img kp canvas)
            (doseq [p (.toList kp)]
              ;(println (.size p))
              )
             canvas)))

    (Imgproc/Canny img-gray canny-output 70 90)
    (debug-fn "Canny output" (constantly canny-output))

    (Imgproc/dilate canny-output canny-output (Mat.))
    (debug-fn "Dilated" (constantly canny-output))

    (if (< (min (.width img) (.height img)) 1000)
      (Imgproc/erode canny-output canny-output (Mat.)))
    (debug-fn "Eroded" (constantly canny-output))

    (Imgproc/findContours canny-output contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_TC89_KCOS (Point. 0 0))
    (debug-fn "Contours" #(Imgproc/drawContours % contours -1 (Scalar. 255 0 0) 1))

    (->> contours
         (filter #(< 250 (Imgproc/contourArea %)))
         ;(filter #(> 0.5 (match-shapes-i1 card-shape (contour-hu-invariants %))))
         (filter rectangle?)
         ;; eliminate overlapping contours
         (reduce (fn [cs c]
                   (if (some #(rect-close-enough? (Imgproc/boundingRect %) (Imgproc/boundingRect c)) cs)
                     cs
                     (conj cs c)))
                 []))))
