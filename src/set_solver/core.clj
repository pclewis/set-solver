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
            [org.opencv.core Core CvType Moments Mat MatOfDouble MatOfByte MatOfInt4 MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria Point Scalar Rect]
            [org.opencv.imgproc Imgproc]
            [org.opencv.imgcodecs Imgcodecs]))

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
  (cond (< hue 25)  (if (< sat 25) :purple :red)
        (< hue 60)  :purple
        (< hue 180) :green
        (< hue 340) :purple
        (< sat 25)  :purple
        :else       :red))

(defn scalar-color-name [color]
  (let [[h s _] (scalar-to-hsl color)]
    (color-name h s)))

(defn shape-name [contour]
  (let [[h1] (contour-hu-invariants contour)]
    (cond (< h1 0.180) :oval
          (< h1 0.200) :diamond
          :else :squiggle)))

(defn find-sets [cards]
  (filter #(->> %
                (map (juxt :shape :color :fill :count))
                transpose
                (map set)
                (map count)
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


(defn find-cards
  "Return set of contours representing cards in img."
  [img]
  (let [img-gray (Mat.)
        canny-output (Mat.)
        hierarchy (MatOfInt4.)
        contours (java.util.LinkedList.)]
    (Imgproc/cvtColor img img-gray Imgproc/COLOR_BGR2GRAY)
    (Imgproc/blur img-gray img-gray (Size. 3 3))
    (Imgproc/Canny img-gray canny-output 60 100)
    (Imgproc/dilate canny-output canny-output (Mat.))
    (if (< (min (.width img) (.height img)) 1000)
      (Imgproc/erode canny-output canny-output (Mat.)))
    (Imgproc/findContours canny-output contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_TC89_KCOS (Point. 0 0))

    (->> contours
         (filter #(< 250 (Imgproc/contourArea %)))
         (filter #(> 0.5 (match-shapes-i1 card-shape (contour-hu-invariants %))))
         (filter rectangle?)
         ;; eliminate overlapping contours
         (reduce (fn [cs c]
                   (if (some #(rect-close-enough? (Imgproc/boundingRect %) (Imgproc/boundingRect c)) cs)
                     cs
                     (conj cs c)))
                 []))))
