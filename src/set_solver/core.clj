(ns set-solver.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :refer [abs round]]
            [com.evocomputing.colors :as colors]
            [clojure.math.combinatorics :as combo]
            [set-solver.reusable-buffer :refer [reusable]]
            [set-solver.match-shapes :refer [match-shapes-i1 match-shapes-i2 match-shapes-i3]])
  (:import  [java.nio ByteBuffer ByteOrder]
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

(defn constrain [n min-n max-n]
  (max (min n max-n) min-n))

(defn transpose [matrix]
  (apply mapv vector matrix))

(defn sgn [n]
  (cond (pos? n) 1
        (neg? n) -1
        :else 0))

(defn load-image []
  (Imgcodecs/imread "resources/overhead.jpg")
  )

(def h (atom nil))
(def c (atom nil))
(def r (atom nil))

(defn close-enough [n1 n2 max-diff]
  (< (Math/abs (- n1 n2)) max-diff))

(defn line-len
  ([p1 p2] (line-len (.x p1) (.y p1) (.x p2) (.y p2)))
  ([p1x p1y p2x p2y]
   (Math/sqrt
    (+ (Math/pow (- p1x p2x) 2)
       (Math/pow (- p1y p2y) 2)))))

(defn center-point
  [pts]
  (let [xs (map #(.x %) pts)
        ys (map #(.y %) pts)
        [min-x max-x] (apply (juxt min max) xs)
        [min-y max-y] (apply (juxt min max) ys)]
    (Point. (+ min-x (/ (- max-x min-x) 2))
            (+ min-y (/ (- max-y min-y) 2)))))

(defn cross-product-z
  [a b]
  (- (* (.x a) (.y b))
     (* (.x b) (.y a))))

(defn orientation [a b c]
  (+ (cross-product-z a b)
     (cross-product-z b c)
     (cross-product-z c a)))

(defn ratio
  ([size] (ratio (.width size) (.height size)))
  ([w h] (/ (max w h) (min w h))))

(defn hierarchy-to-map [h]
  (map (fn [[next-sib prev-sib child parent]]
         {:next-sib next-sib
          :prev-sib prev-sib
          :child child
          :parent parent})
       (partition 4 (.toList h))))

(comment abandoned

         (defn nested-contours
           ([contours hierarchy parent] (nested-contours
                                         (map vector (range) contours (hierarchy-to-map hierarchy))
                                         parent))
           ([ichs parent]
            (let [children (filter (fn [[_ _ h]] (= (:parent h) parent))
                                   ichs)]
              (when children
                (reduce (fn [s [i c h]]
                          (assoc s c (nested-contours ichs i)))
                        {}
                        children))))))

(defn rect-contains [r1 r2]
  (and (.contains r1 (.tl r2))
       (.contains r1 (.br r2))))

(defn rect-contained-in-any [r rs]
  (some #(rect-contains % r) rs))

(defn rects-containing [r rs]
  (apply hash-set (filter #(rect-contains % r) rs)))

;; given vec of [rect data] pairs, return list of {:data data :rect r :children c}
(defn nest-rects [pairs]
  (let [pairs-with-parents
        (map (fn [[rect data]]
               {:rect rect
                :data data
                :parents (rects-containing rect (map first pairs))})
             pairs)]
    (for [p (filter #(empty? (:parents %)) pairs-with-parents)
          :let [children (filter #(some (:parents %) [(:rect p)]) pairs-with-parents)
                children (remove #(some (:parents %) (map :rect children)) children)]]
      (assoc p :children
             (nest-rects (map #(vector (:rect %) (:data %)) children))))))

(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn average [ns]
  (if (zero? (count ns))
    0
    (/ (reduce + ns) (count ns))))

(defn similar-rect [r1 r2]
  (let [max-diff (* 0.10 (line-len (.tl r1) (.br r1)))]
    (and (> max-diff (Math/abs (- (-> r1 .tl .x) (-> r2 .tl .x))))
         (> max-diff (Math/abs (- (-> r1 .tl .y) (-> r2 .tl .y))))
         (> max-diff (Math/abs (- (-> r1 .br .x) (-> r2 .br .x))))
         (> max-diff (Math/abs (- (-> r1 .br .y) (-> r2 .br .y)))))))


(comment
  (similar-rect (Rect. 100 100 100 100) (Rect. 100 100 101 103)))

(defn merge-rects
  "Combine rects where #(pred rect1 rect2) returns true "
  [pred rects]
  (reduce (fn [state rect]
            (let [match (some #(if (pred (:rect rect) (:rect %)) %) state)]
              (if match
                (conj (remove #{match} state)
                      (update-in match [:children] #(merge-rects pred (concat %1 %2)) (:children rect)))
                (conj state rect))))
          []
          rects))

(defn filter-rects
  "Remove rects more than 50% bigger/smaller than the median"
  [rects]
  (let [avg (average (map #(.area (:rect %)) rects))
        min (* 0.5 avg)
        max (* 1.5 avg)]
    (filter #(<= min (.area (:rect %)) max) rects)))


(comment
  (let [rects ( nest-recs
                (map vector
                     (map #(Imgproc/boundingRect %) @c)
                     @c))]
    (map #(.area (:rect %)) (filter-rects rects)))

  )

(defn draw-rects [dst entry color depth]
  (when (and (pos? depth) entry)
    (let [rect (:rect entry)]
      (Imgproc/rectangle dst (.tl rect) (.br rect) color 2))
    (doseq [e (:children entry)]
      (draw-rects dst e color (dec depth)))))

(defn matching-shape? [s1 s2]
  (let [r (Imgproc/matchShapes s1 s2 Imgproc/CV_CONTOURS_MATCH_I3 0.0)]
    (< r 0.1)))


(defn not-average [xs]
  (if (> (count xs) 2)
    (let [sxs (sort xs)
          nxs (rest (drop-last sxs))]
      (/ (reduce + nxs) (count nxs)))
    (/ (reduce + xs) (count xs))))

(defn shape-match-scores
  "Return match score of shapes vs every set in possible-shape-sets"
  [method shapes possible-shape-sets]
  (map (fn [o-shapes]
         (average
          (for [s1 shapes
                s2 o-shapes]
            (Imgproc/matchShapes s1 s2 method 0.0))))
       possible-shape-sets))


(defn best-shape-match
  "Return [set score] of best match in possible-shape-sets"
  [method shapes possible-shape-sets]
  (let [set-scores (map vector possible-shape-sets (shape-match-scores method shapes possible-shape-sets))
        set-scores (sort-by second set-scores)]
    (first set-scores)))

(defn match-shapes [rects method min-score]
  (first
   (reduce (fn [[result known-shapes] rect]
             (let [shapes (->> rect :children (map :data))
                   [best-match best-score] (if (not-empty known-shapes)
                                             (best-shape-match method shapes known-shapes)
                                             [nil 999])]
               (if (< best-score min-score)
                 (let [i (.indexOf known-shapes best-match)]
                   [(conj result (assoc rect :shape i))
                    (update-in known-shapes [i] concat shapes)])
                 [(conj result (assoc rect :shape (count known-shapes)))
                  (conj known-shapes shapes)])))
           [[] []]
           rects)))


(defn color-name [hue]
  (cond
   (< hue 200) :green
   (< hue 275) :red
   :else :purple))

(defn scalar-to-hsl [color]
  (apply colors/rgb-to-hsl (take 3 (.val color))))

(defn scalar-color-name [color]
  (let [[h _ _] (scalar-to-hsl color)]
    (color-name h)))

(def shape-colors
  (map (fn [i]
         (let [[r g b] (colors/hsl-to-rgb (* i 90) 50 50)]
           (Scalar. r g b)))
       (range 20)))

(def match-method Imgproc/CV_CONTOURS_MATCH_I1)
(def match-min 0.15)
(def contour-num 0)

(def contour-thickness -1)

(defn hu-invariants [moments]
  (let [result (reusable (MatOfDouble.))]
    (Imgproc/HuMoments moments result)
    (.toList result)))

(defn contour-hu-invariants [contour]
  (hu-invariants (Imgproc/moments contour)))

(defn shape-name [contour]
  (let [[h1] (contour-hu-invariants contour)]
    (cond (< h1 0.180) :oval
          (< h1 0.200) :diamond
          :else :squiggle)))

(defn contour-poly [contour]
  (let [poly2f (MatOfPoint2f.)
        contour2f (MatOfPoint2f.)
        poly (MatOfPoint.)]
    (.convertTo contour contour2f CvType/CV_32FC2)
    (Imgproc/approxPolyDP contour2f poly2f 1 true)
    (.convertTo poly2f poly CvType/CV_32S)
    poly))

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
          d)
       ])))

(defn find-corners
  [lines]
  (keep (partial apply intersect-lines)
        (combo/combinations lines 2)))


(defn point-add [p1 p2]
  (Point. (+ (.x p1) (.x p2))
          (+ (.y p1) (.y p2))))

(defn point-sub [p1 p2]
  (Point. (- (.x p1) (.x p2))
          (- (.y p1) (.y p2))))

(defn is-quadrilateral [contour]
  (let [c2f (reusable (MatOfPoint2f.))
        approx2f (reusable (MatOfPoint2f.))]
    (.convertTo contour c2f CvType/CV_32FC2)
    (Imgproc/approxPolyDP c2f approx2f
                          (* 0.02 (Imgproc/arcLength c2f true))
                          true)
    (= 4 (.rows approx2f))))




(defn is-rectangle [contour]
  (let [c2f (reusable (MatOfPoint2f.))
        approx2f (reusable (MatOfPoint2f.))]
    (.convertTo contour c2f CvType/CV_32FC2)
    (Imgproc/approxPolyDP c2f approx2f
                          (* 0.02 (Imgproc/arcLength c2f true))
                          true)
    (when (= 4 (.rows approx2f))
      (let [[[p2x p2y] [p1x p1y] [p3x p3y]] (map #(vector (.x %) (.y %)) (.toList approx2f))
            p12 (line-len p1x p1y p2x p2y)
            p13 (line-len p1x p1y p3x p3y)
            p23 (line-len p2x p2y p3x p3y)
            angle (Math/acos
                   (/ (- (+ (* p12 p12) (* p13 p13))
                         (* p23 p23))
                      (* 2 p12 p13)))]
        ;(println angle)
        (< 1.3
           angle
           1.7)))))


(defn find-contours [img]
  (let [img-gray (reusable (Mat.))
        canny-output (reusable (Mat.))
        hierarchy (reusable (MatOfInt4.))
        contours (java.util.LinkedList.)
        drawing (reusable (Mat. (.size img) CvType/CV_8UC3))
        blur-x (constrain (/ (.width img) 500) 3 3)
        blur-y (constrain (/ (.height img) 500) 3 3)]
    (.setTo drawing (Scalar. 0 0 0))
    ;(.copyTo img drawing)
    (Imgproc/cvtColor img img-gray Imgproc/COLOR_BGR2GRAY)
    (Imgproc/blur img-gray img-gray (Size. blur-x blur-y))
    (Imgproc/Canny img-gray canny-output 60 100 ;3 true
                   ) ; 0 600
    ;(Imgproc/blur canny-output canny-output (Size. 5 5))
    (Imgproc/dilate canny-output canny-output (Mat.))
    (if (< (min (.width img) (.height img)) 1000)
      (Imgproc/erode canny-output canny-output (Mat.)))
    (let [m (reusable (Mat. (.size img) CvType/CV_8UC3))]
      (Imgproc/cvtColor canny-output m Imgproc/COLOR_GRAY2BGR)
      (.copyTo m drawing))
    (Imgproc/findContours canny-output contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_TC89_KCOS (Point. 0 0))
    (Imgproc/drawContours drawing contours -1 (Scalar. 255 255 0 ) 3
                                        ;8 hierarchy 1 (Point.)
                            )
    (comment
      (Imgproc/drawContours drawing contours contour-num (Scalar. 255 255 255) contour-thickness
                                        ;8 hierarchy 1 (Point.)
                            ))

    (let [cards (->> contours
                     (filter #(< 250 (Imgproc/contourArea %)))
                     (filter #(> 0.5 (match-shapes-i1 card-shape (contour-hu-invariants %))))
                     (filter is-rectangle)
                     ; eliminate overlapping contours
                     (reduce (fn [cs c]
                               (if (some #(similar-rect (Imgproc/boundingRect %) (Imgproc/boundingRect c)) cs)
                                 cs
                                 (conj cs c)))
                             []))]

      ;(clojure.pprint/pprint (map #(Imgproc/boundingRect %) cards))

      (Imgproc/putText drawing
                       (str (count cards))
                       (Point. 100 100)
                       Core/FONT_HERSHEY_COMPLEX
                       1 (Scalar. 255 255 255) 2)

      (doseq [c cards
              :let [color (Scalar. (rand-int 255) (rand-int 255) (rand-int 255))]]
                                        ;let [c (nth cards contour-num)]
        (Imgproc/putText drawing
                         (str (nth (contour-hu-invariants c) 1))
                         (.tl (Imgproc/boundingRect c))
                         Core/FONT_HERSHEY_COMPLEX
                         1 (Scalar. 255 255 255) 2)
                                        ;(Imgproc/drawContours drawing (list c) 0 color 5)

        ; (Imgproc/drawContours drawing (list c) 0 color -1)



        (let [target (Mat/zeros 600 300 CvType/CV_8UC3)
              target-points (MatOfPoint2f.)
              tright (-> target .cols dec)
              tbottom (-> target .rows dec)
              br (Imgproc/boundingRect c)
              rot (if (> (.width br) (.height br))
                    #(concat (rest %) (take 1 %))
                    identity)
              _ (.fromList target-points (rot (map #(Point. %1 %2) [0 tright tright 0] [0 0 tbottom tbottom])))
              c2f (MatOfPoint2f.)
              approx2f (MatOfPoint2f.)
              approx (MatOfPoint.)
              _ (.convertTo c c2f CvType/CV_32FC2)
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
              trans-mat (Imgproc/getPerspectiveTransform approx2f target-points)]
          (comment
            (Imgproc/circle drawing (first a2f-sorted) 20 color -1)
            (Imgproc/circle drawing center 20 color -1))
          (Imgproc/warpPerspective img target trans-mat (.size target))

         (.copyTo target (.submat drawing (Rect. (.tl (Imgproc/boundingRect c))
                                                  (.size target))))

         (let [target-gray (Mat/zeros (.size target) CvType/CV_8U)
               target-canny (Mat. (.size target) CvType/CV_8U)]
            (Imgproc/cvtColor target target-gray Imgproc/COLOR_BGR2GRAY)

            ;(Imgproc/Canny target-gray target-canny 150 200)
            (Imgproc/blur target-gray target-gray (Size. 2 2))
            (Imgproc/threshold target-gray target-canny 140 255 Imgproc/THRESH_BINARY_INV)
            (Imgproc/rectangle target-canny (Point. 0 0)
                               (.br (Rect. (Point. 0 0) (.size target)))
                               (Scalar. 0 0 0) 50) (println target-canny)
            (let [card-contours (java.util.LinkedList.)
                  card-hierarchy (MatOfInt4.)
                  cmean (Core/mean target target-canny)
                  [edge-h edge-s edge-l] (scalar-to-hsl cmean)
                  cname (scalar-color-name cmean)
                  _ (Imgproc/findContours target-canny card-contours card-hierarchy Imgproc/RETR_EXTERNAL Imgproc/CHAIN_APPROX_SIMPLE)
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
                  fill (cond (and (close-enough inside-l edge-l 5)
                                  (close-enough inside-h edge-h 5))
                             :filled
                             (and (close-enough inside-l outside-l 5)
                                  (close-enough inside-h outside-h 30)
                                  (close-enough inside-s outside-s 5)) :empty
                             :else :shaded)

                  ]

              (Imgproc/putText drawing #_(str (round inside-h) "/"
                                            (round outside-h) "/"
                                            (round edge-h))
                                       (str (count card-contours) cname sname fill)
                               (point-add (Point. 0 700) (.tl (Imgproc/boundingRect c)))
                               Core/FONT_HERSHEY_COMPLEX 2 (Scalar. 255 255 255) 2)

             (comment
               (doseq [cc card-contours]
                 (Imgproc/putText drawing (str (first (contour-hu-invariants cc)))
                                  (point-add
                                   (.tl (Imgproc/boundingRect c))
                                   (.tl (Imgproc/boundingRect cc)))
                                  Core/FONT_HERSHEY_COMPLEX 3 (Scalar. 255 255 255) 2)))

              )
            (Imgproc/threshold target-gray target-canny 140 255 Imgproc/THRESH_BINARY_INV)
            (Imgproc/rectangle target-canny (Point. 0 0)
                               (.br (Rect. (Point. 0 0) (.size target)))
                               (Scalar. 0 0 0) 50) (println target-canny)

            (Imgproc/cvtColor target-canny target Imgproc/COLOR_GRAY2BGR)
            (.copyTo target (.submat drawing (Rect. (point-add (Point. 300 0)
                                                               (.tl (Imgproc/boundingRect c)))
                                                  (.size target))))

            )
          ;(.copyTo target drawing)

          )

        (comment
          (let [c2f (MatOfPoint2f.)
                approx2f (MatOfPoint2f.)
                approx (MatOfPoint.)]
            (.convertTo c c2f CvType/CV_32FC2)
            (Imgproc/approxPolyDP c2f approx2f
                                  (* 0.02 (Imgproc/arcLength c2f true))
                                  true)
            (.convertTo approx2f approx CvType/CV_32S)
            (when (and (= 4 (count (.toList approx))))
              (comment
                (println
                 (-> approx Imgproc/boundingRect .size ratio)))
              (doseq [pt (.toList approx)]
                (Imgproc/circle drawing pt 20 color -1))
              (doseq [[p1 p2] (combo/combinations (.toList approx) 2)]
                (Imgproc/line drawing p1 p2 color 3)))))

        (comment
          (let[lines (MatOfInt4.)
               rect (Imgproc/boundingRect c)
               rtl (.tl rect)
                                        ;rect
               #_(Rect. (point-sub (.tl rect) (Point. 10 10))
                        (point-add (.br rect) (Point. 10 10)))
               i-rect (Rect. (point-sub (.tl rect) (Point. 250 250))
                             (point-add (.br rect) (Point. 250 250)))
               _ (println rect)
               tmp (Mat/zeros (.size rect) CvType/CV_8U)
               _ (Imgproc/drawContours tmp (list c) 0 (Scalar. 255 255 255) 1
                                       8 (Mat.) 1 (point-sub (Point. 10 10) rtl))
               _ (Imgproc/dilate tmp tmp (Mat.))
                                        ;_ (Imgproc/drawContours drawing (list c) 0 (Scalar. 255 255 255) 2)
               _ (Imgproc/HoughLinesP tmp lines 2 (/ Math/PI 180) 70 200 10)
               lines (partition 4 (.toList lines))
               corners (find-corners lines)
               corners (filter #(.contains i-rect (point-add rtl (Point. (first %) (second %)))) corners)
               corner-mat (MatOfPoint2f.)
               corner-matp (MatOfPoint.)
               c2f (MatOfPoint2f.)
               approx2f (MatOfPoint2f.)
               approx (MatOfPoint.)]
            (when (not-empty corners)

              (comment
                (doseq [line lines]
                  (Imgproc/line (.submat drawing rect)
                                (Point. (nth line 0) (nth line 1))
                                (Point. (nth line 2) (nth line 3))
                                (Scalar. (rand-int 255) (rand-int 255) (rand-int 255))
                                1)))
              (comment
                (doseq [[l1 l2] (combo/combinations lines 2)
                        :let [corner (intersect-lines l1 l2)
                              color (Scalar. (rand-int 255) (rand-int 255) (rand-int 255))]]
                  (when (and corner
                             (.contains i-rect (point-add rtl (Point. (first corner) (second corner))))
                             )
                    (Imgproc/rectangle drawing (.tl rect) (.br rect) (Scalar. 255 255 255) 1)
                    (Imgproc/circle drawing (point-add rtl (apply #(Point. %1 %2) corner)) 20 color)
                    (Imgproc/line drawing
                                  (point-add rtl (Point. (nth l1 0) (nth l1 1)))
                                  (point-add rtl (Point. (nth l1 2) (nth l1 3)))
                                  color
                                  2)
                    (Imgproc/line drawing
                                  (point-add rtl (Point. (nth l2 0) (nth l2 1)))
                                  (point-add rtl (Point. (nth l2 2) (nth l2 3)))
                                  color
                                  2)
                    (Imgproc/line drawing
                                  (point-add rtl (Point. (nth l1 0) (nth l1 1)))
                                  (point-add rtl (Point. (nth corner 0) (nth corner 1)))
                                  color
                                  1)
                    (Imgproc/line drawing
                                  (point-add rtl (Point. (nth l2 0) (nth l2 1)))
                                  (point-add rtl (Point. (nth corner 0) (nth corner 1)))
                                  color
                                  1))))
              (comment
                (doseq [pt corners]
                  (Imgproc/circle drawing (point-add rtl (apply #(Point. %1 %2) pt)) 20 color)))
              (.convertTo corner-mat corner-matp CvType/CV_32S)
                                        ;(Imgproc/drawContours (.submat drawing rect) (list corner-matp) 0 color -1 )
                                        ;(.fromList corner-mat (flatten (map #(Point. (first %) (second %)) corners)))
                                        ;(Imgproc/drawContours (.submat drawing rect) (list approx) 0 color 5)
              )
            (comment
              )

            (comment
              (when (< (count lines) 50)
                ))))


        ))

    (comment
      (let [pairs (map vector
                       (map #(Imgproc/boundingRect %) contours)
                       contours)
            rects (filter-rects (nest-rects pairs))
                                        ;rects (merge-rects similar-rect rects)
                                        ;rects (match-shapes rects match-method match-min)
            shapes (distinct (map :shape rects))
            shape-color-map (zipmap shapes shape-colors)
            ]
        (compare-and-set! r @r rects)
        (doseq [[entry i] (map vector rects (range))
                :let [rect (:rect entry)
                                        ;cv (mod (* (.x (.tl rect)) 123 (.y (.tl rect))) 360)
                                        ;[r g b] (hsl-to-rgb cv 50 50)
                      first-child (-> entry :children first)
                                        ;fc-contour-point (-> (:data first-child) .toList first)
                      fc-contour-mask (Mat/zeros (.size img) CvType/CV_8U)
                      _ (when first-child (Imgproc/drawContours fc-contour-mask
                                                                (list (:data first-child))
                                                                0
                                                                (Scalar. 255 255 255)
                                                                1))
                      fc-inner-mask (Mat/zeros (.size img) CvType/CV_8U)
                      _ (doseq [child (:children entry)]
                          (let [poly2f (MatOfPoint2f.)
                                child2f (MatOfPoint2f.)
                                poly (MatOfPoint.)]
                            (.convertTo (:data child) child2f CvType/CV_32FC2)
                            (Imgproc/approxPolyDP child2f poly2f 0 true)
                            (.convertTo poly2f poly CvType/CV_32S)
                            (Imgproc/drawContours fc-inner-mask
                                                  (list poly)
                                                  0
                                                  (Scalar. 255 255 255)
                                                  -1)))
                      child-moments (map #(Imgproc/moments (:data %)) (:children entry))
                      child-hu-invs (map #(hu-invariants %) child-moments)
                      avg-invs (average (map first child-hu-invs))
                      avg-ecc (average (map #(/ (second %) (Math/pow (first %) 2)) child-hu-invs))
                                        ;color (Scalar. (.get img (.y fc-contour-point) (.x fc-contour-point)))
                                        ;color (Core/mean (.submat img (:rect first-child)))
                      color (Core/mean img fc-contour-mask)
                      [r g b _] (.val color)
                      [h s l] (colors/rgb-to-hsl r g b)
                      [r2 g2 b2] (colors/hsl-to-rgb h 100 50)
                      color2 (Scalar. r2 g2 b2)
                      color3 (get shape-color-map (:shape entry))
                                        ;color (Scalar. r g b)
                      info (str i #_(color-name h) "," (count (:children entry)) "," (:shape entry) "," (-> avg-ecc (* 1000) round))
                      ]]
                                        ;(Imgproc/putText drawing info (.tl rect) Core/FONT_HERSHEY_COMPLEX 1 color 2)
                                        ;(draw-rects drawing entry color3 5)
                                        ;(.copyTo img drawing fc-inner-mask)
          (comment
            (doseq [child (:children entry)
                    :let [rect (:rect child)]]
              (Imgproc/drawContours drawing (list (:data child)) 0 color2 2)
              (comment
                (-> (.submat img rect)
                    (.copyTo (.submat drawing rect)))))))))

    (compare-and-set! h @h hierarchy)
    (compare-and-set! c @c contours)
    drawing
    ))

(defn find-cards [img]
  (let [img-gray (Mat.)
        canny-output (Mat.)
        hierarchy (MatOfInt4.)
        contours (java.util.LinkedList.)
        drawing (Mat/zeros (.size img) CvType/CV_8UC3)]
    (Imgproc/cvtColor img img-gray Imgproc/COLOR_BGR2GRAY)
    (Imgproc/blur img-gray img-gray (Size. 3 3))
    (Imgproc/Canny img-gray canny-output 100 200) ; 0 600
    (Imgproc/findContours canny-output contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_SIMPLE (Point. 0 0))
    (let [pairs (map vector
                     (map #(Imgproc/boundingRect %) contours)
                     contours)]
      (nest-rects pairs))))

(defn mat->p-img [{:keys [canny in-mat tmp-mat out-mat b-array i-array p-img top left zoom]}]
  ;
  (let [rm (Mat.)
        _ (Imgproc/resize canny rm (Size.) zoom zoom Imgproc/INTER_AREA)
        rw (min width (.width rm))
        rh (min height (.height rm))
        rs (Size. rw rh)
        rect (Rect. (Point. (constrain left 0 (- (.width rm) width))
                            (constrain top 0 (- (.height rm) height)))
                    rs) ]
    (.copyTo (.submat rm rect)
             (.submat tmp-mat (Rect. (Point. 0 0) rs)))
    )
  (Imgproc/cvtColor tmp-mat out-mat Imgproc/COLOR_RGB2RGBA 4)
  (.get out-mat 0 0 b-array)
  (-> (ByteBuffer/wrap b-array)
      (.order ByteOrder/LITTLE_ENDIAN)
      (.asIntBuffer)
      (.get i-array))
  (.loadPixels p-img)
  (set! (.pixels p-img) (aclone i-array))
  (.updatePixels p-img)
  p-img)

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate frame-rate)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (let [image (load-image)
        state {:mat-converter {:in-mat  image
                               :tmp-mat (Mat. height width CvType/CV_8UC3)
                               :out-mat (Mat. height width CvType/CV_8UC4)
                               :b-array (byte-array (* 4 n-pixels))
                               :i-array (int-array n-pixels)
                               :p-img   (q/create-image width height :rgb)
                               :top 0
                               :left 0
                               :zoom 1.0}
               :color 0
               :angle 0
               }
        state (assoc-in state [:mat-converter :canny] (find-contours image))]
   (mat->p-img (:mat-converter state))
    state))

(defn update-state [state]
  ; Update sketch state by changing circle color and position.
  (let [state (assoc-in state [:mat-converter :canny] (find-contours (get-in state [:mat-converter :in-mat])))
        state (assoc-in state [:mat-converter :p-img] (mat->p-img (:mat-converter state)))
        state (into state {:color (mod (+ (:color state) 0.7) 255)
                           :angle (+ (:angle state) 0.1)})]
    state))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
;  (q/background 128)
  ; Set circle color.

  (q/background 0)
  (q/image (:p-img (:mat-converter state)) 1 1)
  (q/fill 0 0 0 192)
  (q/rect 0 0 300 60)
  (q/fill 255)
  (q/text (str (select-keys (:mat-converter state) [:left :top :zoom])) 15 15)
  (q/text (str (round (q/current-frame-rate)) "/" (q/target-frame-rate)) 15 45)
  (q/fill (:color state) 255 255)
                                        ; Calculate x and y coordinates of the circle.
  (let [angle (:angle state)
        x (* 150 (q/cos angle))
        y (* 150 (q/sin angle))]
                                        ; Move origin point to the center of the sketch.
    (q/with-translation [(/ (q/width) 2)
                         (/ (q/height) 2)]
                                        ; Draw the circle.
      (q/ellipse x y 10 10))))

(defn key-pressed [state e]
  (q/frame-rate frame-rate)
  (case (:key e)
    :j (update-in state [:mat-converter :top] #(+ 15 %))
    :k (update-in state [:mat-converter :top] #(- % 15))
    :l (update-in state [:mat-converter :left] #(+ 15 %))
    :h (update-in state [:mat-converter :left] #(- % 15))
    :+ (update-in state [:mat-converter :zoom] #(* % 2))
    :- (update-in state [:mat-converter :zoom] #(/ % 2))
    state))

(q/defsketch set-solver
  :title "You spin my circle right round"
  :size [width height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  :key-pressed key-pressed
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(comment

  (clojure.pprint/pprint
   (filter #(< 0.174 (last %) 0.180)
    (map #(identity [(-> % Imgproc/boundingRect .tl)
                     (-> % Imgproc/boundingRect .size ratio)
                     (first (contour-hu-invariants %))])
         (filter #(< 1.2 (-> % Imgproc/boundingRect .size ratio) 1.6) @c))))


  )
