(ns set-solver.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :refer [abs round]])
  (:import  [java.nio ByteBuffer ByteOrder]
            [org.opencv.core Core CvType Mat MatOfByte MatOfInt4 MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria Point Scalar]
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
(def width 640)
(def height 480)
(def n-pixels (* width height))

(defn load-image []
  (let [res (Mat.)]
    (Imgproc/resize (Imgcodecs/imread "resources/set1.jpg") res (Size.) 0.3 0.3 Imgproc/INTER_AREA)
    res))

(def h (atom nil))
(def c (atom nil))

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

(comment
  ( nest-recs
    (map vector
         (map #(Imgproc/boundingRect %) @c)
         @c))

  )

(defn draw-rects [dst entry color depth]
  (when (and (pos? depth) entry)
    (let [rect (:rect entry)]
      (Imgproc/rectangle dst (.tl rect) (.br rect) color 2))
    (doseq [e (:children entry)]
      (draw-rects dst e color (dec depth)))))


(defn find-contours [img]
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
                     contours)
          rects (nest-rects pairs)]
      (doseq [entry rects
              :let [rect (:rect entry)
                    ;cv (mod (* (.x (.tl rect)) 123 (.y (.tl rect))) 360)
                    ;[r g b] (hsl-to-rgb cv 50 50)
                    color (Core/mean (.submat img (-> entry :children first :rect)))
                    ;color (Scalar. r g b)
                    ]]
        (draw-rects drawing entry color 2)
        (doseq [rect (map :rect (:children entry))]
          (-> (.submat img rect)
              (.copyTo (.submat drawing rect))))))

    (compare-and-set! h @h hierarchy)
    (compare-and-set! c @c contours)
    drawing
    ))

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
