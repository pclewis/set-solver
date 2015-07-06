(ns set-solver.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :refer [abs round]]
            [com.evocomputing.colors :as colors]
            [set-solver.reusable-buffer :refer [reusable]])
  (:import  [java.nio ByteBuffer ByteOrder]
            [org.opencv.core Core CvType Moments Mat MatOfDouble MatOfByte MatOfInt4 MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria Point Scalar Rect]
            [org.opencv.imgproc Imgproc]
            [org.opencv.imgcodecs Imgcodecs]))

(defn hue-to-rgb
  "Convert hue color to rgb components
  Based on algorithm described in:
  http://en.wikipedia.org/wiki/Hue#Computing_hue_from_RGB
  and:
  http://www.w3.org/TR/css3-color/#hsl-color"
  [m1, m2, hue]
  (let* [h (cond
           (< hue 0) (inc hue)
           (> hue 1) (dec hue)
           :else hue)]
        (cond
         (< (* h 6) 1) (+ m1 (* (- m2 m1) h 6))
         (< (* h 2) 1) m2
         (< (* h 3) 2) (+ m1 (* (- m2 m1) (- (/ 2.0 3) h) 6))
         :else m1)))

(defn hsl-to-rgb
  "Given color with HSL values return vector of r, g, b.
  Based on algorithms described in:
  http://en.wikipedia.org/wiki/Luminance-Hue-Saturation#Conversion_from_HSL_to_RGB
  and:
  http://en.wikipedia.org/wiki/Hue#Computing_hue_from_RGB
  and:
  http://www.w3.org/TR/css3-color/#hsl-color"
  [hue saturation lightness]
  (let* [h (/ hue 360.0)
         s (/ saturation 100.0)
         l (/ lightness 100.0)
         m2 (if (<= l 0.5) (* l (+ s 1))
                (- (+ l s) (* l s)))
         m1 (- (* l 2) m2)]
        (into []
              (map #(round (* 0xff %))
                   [(hue-to-rgb m1 m2 (+ h (/ 1.0 3)))
                    (hue-to-rgb m1 m2 h)
                    (hue-to-rgb m1 m2 (- h (/ 1.0 3)))]))))

(def frame-rate 30)
(def width 1024)
(def height 768)
(def n-pixels (* width height))

(defn load-image []
  (let [res (Mat.)]
    ;(Imgproc/resize (Imgcodecs/imread "resources/set1.jpg") res (Size.) 0.3 0.3 Imgproc/INTER_AREA)
    (Imgcodecs/imread "resources/set2.jpg")
    res))

(def h (atom nil))
(def c (atom nil))
(def r (atom nil))

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
  (let [max-diff 15.0]
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

(def shape-colors
  (map (fn [i]
         (let [[r g b] (hsl-to-rgb (* i 90) 50 50)]
           (Scalar. r g b)))
       (range 20)))

(def match-method Imgproc/CV_CONTOURS_MATCH_I1)
(def match-min 0.15)

(def contour-num -1)

(def contour-thickness 1)

(defn hu-invariants [moments]
  (let [result (reusable (MatOfDouble.))]
    (Imgproc/HuMoments moments result)
    (.toList result)))

(defn find-contours [img]
  (let [img-gray (reusable (Mat.))
        canny-output (reusable (Mat.))
        hierarchy (reusable (MatOfInt4.))
        contours (java.util.LinkedList.)
        drawing (reusable (Mat. (.size img) CvType/CV_8UC3))]
    (.setTo drawing (Scalar. 0 0 0))
    ;(.copyTo img drawing)
    (Imgproc/cvtColor img img-gray Imgproc/COLOR_BGR2GRAY)
    (Imgproc/blur img-gray img-gray (Size. 2 2))
    (Imgproc/Canny img-gray canny-output 100 200) ; 0 600
    ;(Imgproc/dilate canny-output canny-output (Mat.))
    ;(Imgproc/erode canny-output canny-output (Mat.))
    (comment
      (let [m (reusable (Mat. (.size img) CvType/CV_8UC3))]
        (Imgproc/cvtColor canny-output m Imgproc/COLOR_GRAY2BGR)
        (.copyTo m drawing)))
    (Imgproc/findContours canny-output contours hierarchy Imgproc/RETR_TREE Imgproc/CHAIN_APPROX_SIMPLE (Point. 0 0))
    (Imgproc/drawContours drawing contours contour-num (Scalar. 255 255 255) contour-thickness
                          ;8 hierarchy 1 (Point.)
                          )

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

(defn mat->p-img [{:keys [canny in-mat tmp-mat out-mat b-array i-array p-img]}]
  (Imgproc/resize canny tmp-mat (.size tmp-mat) 0 0 Imgproc/INTER_AREA)
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
                               :p-img   (q/create-image width height :rgb)}
               :color 0
               :angle 0}
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

(q/defsketch set-solver
  :title "You spin my circle right round"
  :size [width height]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
