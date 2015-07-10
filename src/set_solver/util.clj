(ns set-solver.util
  (:require [com.evocomputing.colors :as colors]
            [clojure.math.combinatorics :as combo]
            [set-solver.reusable-buffer :refer [reusable]])
  (:import [org.opencv.core CvType Point MatOfDouble MatOfPoint2f]
           [org.opencv.imgproc Imgproc] ) )

(defn constrain
  "Force number into range"
  [n min-n max-n]
  (max (min n max-n) min-n))

(defn transpose
  "Tranpose a matrix. Ex: [ [A B] [C D] ] to [ [A C] [B D] ] "
  [matrix]
  (apply mapv vector matrix))

(defn sgn [n]
  "1 if n is positive, -1 is n is negative, 0 is n is zero"
  (cond (pos? n) 1
        (neg? n) -1
        :else 0))

(defn close-enough?
  "Return true if the difference between n1 and n2 is less than max-diff."
  [n1 n2 max-diff]
  (< (Math/abs (- n1 n2)) max-diff))

(defn line-len
  "Calculate length of line given two Points or 4 coordinates"
  ([p1 p2] (line-len (.x p1) (.y p1) (.x p2) (.y p2)))
  ([p1x p1y p2x p2y]
   (Math/sqrt
    (+ (Math/pow (- p1x p2x) 2)
       (Math/pow (- p1y p2y) 2)))))

(defn center-point
  "Return the center Point of a collection of points."
  [pts]
  (let [xs (map #(.x %) pts)
        ys (map #(.y %) pts)
        [min-x max-x] (apply (juxt min max) xs)
        [min-y max-y] (apply (juxt min max) ys)]
    (Point. (+ min-x (/ (- max-x min-x) 2))
            (+ min-y (/ (- max-y min-y) 2)))))

(defn rect-ratio
  "The ratio of a rectangle's smaller larger side."
  ([size] (rect-ratio (.width size) (.height size)))
  ([w h] (/ (max w h) (min w h))))

(defn rect-contains?
  "True if r1 contains r2. Note rects do not 'contain' their own bottom right, so (rect-contains? r r) is false."
  [r1 r2]
  (and (.contains r1 (.tl r2))
       (.contains r1 (.br r2))))

(defn rect-close-enough?
  "True if the top-left and bottom-right points of r1 and r2 differ by less than max-diff (default 10% of cross length)"
  ([r1 r2] (rect-close-enough? r1 r2 (* 0.10 (line-len (.tl r1) (.br r1)))))
  ([r1 r2 max-diff]
   (and (close-enough? (-> r1 .tl .x) (-> r2 .tl .x) max-diff)
        (close-enough? (-> r1 .tl .y) (-> r2 .tl .y) max-diff)
        (close-enough? (-> r1 .br .x) (-> r2 .br .x) max-diff)
        (close-enough? (-> r1 .br .y) (-> r2 .br .y) max-diff))))

(defn scalar-to-hsl
  "Convert a scalar RGB color to HSL"
  [color]
  (apply colors/rgb-to-hsl (reverse (take 3 (.val color)))))

(defn hu-invariants
  "Calculate Hu invariants from moments"
  [moments]
  (let [result (reusable (MatOfDouble.))]
    (Imgproc/HuMoments moments result)
    (.toList result)))

(defn contour-hu-invariants
  "Calculate Hu invariants from contour"
  [contour]
  (hu-invariants (Imgproc/moments contour)))

(defn intersect-lines
  "Return the point where two lines intersect, or null if they are parallel/coincident"
  [line1 line2]
  (let [[x1 y1 x2 y2] (map double line1)
        [x3 y3 x4 y4] (map double line2)
        d (- (* (- x1 x2) (- y3 y4))
             (* (- y1 y2) (- x3 x4)))]
    (when-not (zero? d)
      [(/ (- (* (- (* x1 y2) (* y1 x2))
                (- x3 x4))
             (* (- x1 x2)
                (- (* x3 y4) (* y3 x4))))
          d)
       (/ (- (* (- (* x1 y2) (* y1 x2))
                (- y3 y4))
             (* (- y1 y2)
                (- (* x3 y4) (* y3 x4))))
          d)])))

(defn find-corners
  "Find all points where given set of lines would intersect."
  [lines]
  (keep (partial apply intersect-lines)
        (combo/combinations lines 2)))

(defn point-add
  "Add p1 to p2"
  [p1 p2]
  (Point. (+ (.x p1) (.x p2))
          (+ (.y p1) (.y p2))))

(defn point-sub
  "Subtract p2 from p1"
  [p1 p2]
  (Point. (- (.x p1) (.x p2))
          (- (.y p1) (.y p2))))

(defn quadrilateral?
  "True if contour can be approximated by a polygon with 4 points."
  [contour]
  (let [c2f (MatOfPoint2f.)
        approx2f (MatOfPoint2f.)]
    (.convertTo contour c2f CvType/CV_32FC2)
    (Imgproc/approxPolyDP c2f approx2f
                          (* 0.02 (Imgproc/arcLength c2f true))
                          true)
    (= 4 (.rows approx2f))))

(defn sort-rectangle-points
  "Sort a list of points in specified order
   Default: [top-left top-right bottom-right bottom-left]"
  ([pts] (sort-rectangle-points pts [[-1 -1] [1 -1] [1 1] [-1 1]]))
  ([pts pt-order]
   (let [center (center-point pts)]
     (sort-by #(.indexOf pt-order
                         [(sgn (- (.x %) (.x center)))
                          (sgn (- (.y %) (.y center)))])
              pts))))

(defn angle-3p
  "Given points A B C defining lines AB and BC, the angle at B"
  [a b c]
  (let [[[p2x p2y] [p1x p1y] [p3x p3y]] (map #(vector (.x %) (.y %)) [a b c])
        p12 (line-len p1x p1y p2x p2y)
        p13 (line-len p1x p1y p3x p3y)
        p23 (line-len p2x p2y p3x p3y)]

    ;; acos((p12^2 + p13^2) - (p23^2)) / (2 * p12 * p13)
    (Math/acos
     (/ (- (+ (* p12 p12)
              (* p13 p13))
           (* p23 p23))
        (* 2 p12 p13)))))

(defn sort-rectangle-points-angle
  [pts]
   (let [center (center-point pts)]
     (sort-by #(Math/atan2 (- (.y %) (.y center))
                           (- (.x center) (.x %)))
              pts)) )

(defn connected-combinations
  [coll n]
  (let [lc (cycle coll)]
    (apply map vector coll (map #(drop (inc %) lc) (range (dec n))))))

(defn pts-rectangle?
  [pts]
  (let [[angle1 angle2 angle3 angle4 :as angles]
        (map #(apply angle-3p %) (connected-combinations pts 3))]
    (and (every? #(close-enough? % (/ Math/PI 2) (/ Math/PI 8)) angles)
         (close-enough? angle1 angle3 0.2)
         (close-enough? angle2 angle4 0.2)
         (close-enough? (+ angle1 angle2) (Math/PI) 0.1))) )

(defn rectangle?
  "True if contour can be approximated by a polygon with 4 points that meet at more-or-less right angles."
  [contour]
  (let [c2f (reusable (MatOfPoint2f.))
        approx2f (reusable (MatOfPoint2f.))]
    (.convertTo contour c2f CvType/CV_32FC2)
    (Imgproc/approxPolyDP c2f approx2f
                          (* 0.02 (Imgproc/arcLength c2f true))
                          true)
    (when (= 4 (.rows approx2f))
      (pts-rectangle? (sort-rectangle-points-angle (.toList approx2f))))))

(defn enumerate
  "Lazy seq of [i x] for each x in xs and i in (range)"
  [xs]
  (map-indexed vector xs))
