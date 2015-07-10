(ns set-solver.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :refer [abs round]]
            [com.evocomputing.colors :as colors]
            [clojure.math.combinatorics :as combo]
            [set-solver.reusable-buffer :refer [reusable]]
            [set-solver.match-shapes :refer [match-shapes-i1 match-shapes-i2 match-shapes-i3]]
            [set-solver.util :refer :all])

  (:import [java.nio ByteBuffer ByteOrder]
            [org.opencv.core Core CvType Moments Mat MatOfDouble MatOfByte MatOfInt4 MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria Point Scalar Rect MatOfKeyPoint]
            [org.opencv.imgproc Imgproc]
            [org.opencv.imgcodecs Imgcodecs]
            [org.opencv.features2d FeatureDetector Features2d]))

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

(defn identify-card
  [img contour]
  (let [target (Mat/zeros 600 300 CvType/CV_8UC3)
        target-points (MatOfPoint2f.)
        tright (-> target .cols dec)
        tbottom (-> target .rows dec)
        br (Imgproc/boundingRect contour)
        rot (if (> (.width br) (.height br))
              #(concat (rest %) (take 1 %))
              identity)
        _ (.fromList target-points (rot (map #(Point. %1 %2) [0 tright tright 0] [0 0 tbottom tbottom])))
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
        pt-order [[-1 -1] [1 -1] [1 1] [-1 1]]
        a2f-sorted (sort-by #(.indexOf pt-order
                                       [(sgn (- (.x %) (.x center)))
                                        (sgn (- (.y %) (.y center)))]) approx2fl)
        _ (.fromList approx2f a2f-sorted)
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
          card-contours (filter #(< 200 (Imgproc/contourArea %)) card-contours)
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

(defn- debug-rect [img debug-fn contours]
  (debug-fn
   (fn []
     (let [canvas (Mat/zeros (.size img) CvType/CV_8U)]
       (doseq [contour contours]
         (let [
               c2f (MatOfPoint2f.)
               approx2f (MatOfPoint2f.)
               approx (MatOfPoint.)
               _ (.convertTo contour c2f CvType/CV_32FC2)
               _ (comment (.push_back c2f (doto (MatOfPoint2f.)
                                            (.fromList (take 1 (.toList c2f))))))
               _ (Imgproc/approxPolyDP c2f approx2f
                                       (* 0.02 (Imgproc/arcLength c2f true))
                                       true)
               _ (.convertTo approx2f approx CvType/CV_32S)
               color (case (round (Math/abs (- 4 (.rows approx))))
                       0 (Scalar. 255 0 0)
                       1 (Scalar. 192 0 0)
                       2 (Scalar. 128 0 0)
                       (Scalar. 32 0 0))]
           (Imgproc/drawContours canvas (list approx) 0 color)
           (when (= 4 (.rows approx2f))
             (let [pts (sort-rectangle-points-angle (.toList approx2f))
                   angle1 (apply angle-3p (take 3 pts))
                   angle2 (apply angle-3p (take-last 3 pts))]
               (Imgproc/circle canvas (nth pts 1) 6 (Scalar. 255 0 0))
               (Imgproc/circle canvas (nth pts 2) 6 (Scalar. 255 0 0) -1)
               (Imgproc/circle canvas (nth pts 3) 6 (Scalar. 255 0 0))
               (Imgproc/putText canvas (format "%.1f" angle1) (nth pts 2)
                                Core/FONT_HERSHEY_COMPLEX 1 (Scalar. 255 0 0))))))
                canvas))
            "rectangles")
  contours)


(defn- find-cards
  [img debug-fn]
  (let [contours (java.util.LinkedList.)]
    (Imgproc/findContours img contours (MatOfInt4.)
                          Imgproc/RETR_LIST
                          Imgproc/CHAIN_APPROX_SIMPLE)
   (debug-fn
     (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)]
             (Imgproc/drawContours canvas contours -1
                                   (Scalar. 255 0 0) 1)
             canvas))
     "Contours-pre")

    (let [contours
          (->> contours
               (filter #(< 250 (Math/abs (Imgproc/contourArea %))))
               (filter #(> 0.4 (match-shapes-i1 card-shape (contour-hu-invariants %))))
               (debug-rect img debug-fn)
               (filter rectangle?)
               distinct-contours
               )]
      (debug-fn
       (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)]
               (doseq [[i c] (enumerate contours)]
                 (Imgproc/drawContours canvas contours i
                                       (Scalar. (rand-int 255) 0 0) 1))
               canvas))
       "Contours-post")
      contours)))

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
    (debug-fn
       (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)]
               (Features2d/drawKeypoints img kp canvas)
               canvas))
       "keypoints")
    (Imgproc/cvtColor img img-hsl Imgproc/COLOR_RGB2HSV)
    (Core/split img-hsl channels)
    (debug-fn (constantly (.get channels 0)) "hue")
    (debug-fn (constantly (.get channels 1)) "saturation")
    (debug-fn (constantly (.get channels 2)) "value")
    (Imgproc/threshold (.get channels 1) sat-mask 60 255
                       Imgproc/THRESH_BINARY_INV)
    (debug-fn (constantly sat-mask) "satmask")


    ;; FIXME ugly
    (let [result
          (flatten
           (doall
            (for [[i p] (enumerate (.toList kp))]
              (do
                (.setTo mask (Scalar. 0 0 0))
                (Imgproc/circle mask (.pt p) 30 (Scalar. 255 255 255) 3)
                (Core/multiply sat-mask mask mask)
                (debug-fn (constantly mask) (str "Blobmask" i))
                (let [avg (Core/mean img mask)
                      canny (Mat.)]
                  (Core/absdiff img avg abs-diff)
                  (debug-fn (constantly abs-diff) (str "absdiff" i))
                  (Imgproc/cvtColor abs-diff abs-diff Imgproc/COLOR_BGR2GRAY)
                  (Imgproc/threshold abs-diff abs-diff 60 255
                                     Imgproc/THRESH_BINARY_INV)
                  ;(Imgproc/Canny abs-diff canny 40 50)
                  (Imgproc/erode abs-diff canny (Mat.))
                  (debug-fn (constantly canny) (str "canny" i))
                  (Imgproc/dilate canny canny (Mat.))
                  (find-cards canny #(debug-fn %1 (str %2 "-" i))))))))]

      (debug-fn
       (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)]
               (Imgproc/drawContours canvas result -1 (Scalar. 255 0 0) 1)
               canvas))
       "FINAL Contours")

      result)

    (comment ;; all blobs at once
     (doseq [p (.toList kp)]
        (Imgproc/circle mask (.pt p) 30 (Scalar. 255 255 255) 3))
      (Core/multiply sat-mask mask mask)
      (debug-fn (constantly mask) "Blob mask")
      (let [avg (Core/mean img mask)
            canny (Mat.)]
        (Core/absdiff img avg abs-diff)
        (Imgproc/cvtColor abs-diff abs-diff Imgproc/COLOR_BGR2GRAY)
        (Imgproc/threshold abs-diff abs-diff 40 255
                           Imgproc/THRESH_BINARY_INV)
        (debug-fn (constantly abs-diff) "abs-diff")
        (Imgproc/Canny abs-diff canny 40 50)
        (debug-fn (constantly canny) "canny")
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
    (debug-fn (constantly img-gray) "Grayscale image")

    (Imgproc/cvtColor img img-hsl Imgproc/COLOR_RGB2HSV)
    (Core/split img-hsl channels)
    (debug-fn (constantly (.get channels 0)) "hue")
    (debug-fn (constantly (.get channels 1)) "saturation")
    (debug-fn (constantly (.get channels 2)) "value")
    (Core/add (.get channels 1) (.get channels 2) img-sl)
    (debug-fn (constantly img-sl) "plus")

    (Core/split img channels)
    (debug-fn (constantly (.get channels 0)) "red")
    (debug-fn (constantly (.get channels 1)) "green")
    (debug-fn (constantly (.get channels 2)) "blue")

    (Core/divide (.get channels 0) (Scalar. 3 0 0) (.get channels 0))
    (Core/divide (.get channels 1) (Scalar. 3 0 0) (.get channels 1))
    (Core/divide (.get channels 2) (Scalar. 3 0 0) (.get channels 2))
    (Core/add (.get channels 0) (.get channels 1) img-intensity)
    (Core/add img-intensity (.get channels 2) img-intensity)
    (debug-fn (constantly img-intensity) "intensity-base")

   (debug-fn
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
             canvas))
     "Intensity-adaptive")

   (comment
     (let [fd (FeatureDetector/create FeatureDetector/SIMPLEBLOB)]
       (.write fd "test.yaml"))

     )

   (debug-fn
    (fn [] (let [canvas (Mat/zeros (.size img) CvType/CV_8U)
                fd (FeatureDetector/create FeatureDetector/SIMPLEBLOB)
                kp (MatOfKeyPoint.)]
            (.read fd "test.yaml")
            (.detect fd img kp)
            (Features2d/drawKeypoints img kp canvas)
            (doseq [p (.toList kp)]
              ;(println (.size p))
              )
             canvas))
     "Blobs")

    (Imgproc/Canny img-gray canny-output 70 90)
    (debug-fn (constantly canny-output) "Canny output")

    (Imgproc/dilate canny-output canny-output (Mat.))
    (debug-fn (constantly canny-output) "Dilated")

    (if (< (min (.width img) (.height img)) 1000)
      (Imgproc/erode canny-output canny-output (Mat.)))
    (debug-fn (constantly canny-output) "Eroded")

    (Imgproc/findContours canny-output contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_TC89_KCOS (Point. 0 0))
    (debug-fn
     (fn [] (let [canvas (Mat/zeros (.size canny-output) CvType/CV_8U)]
             (Imgproc/drawContours canvas contours -1 (Scalar. 255 0 0) 1)
             canvas))
     "Contours")

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
