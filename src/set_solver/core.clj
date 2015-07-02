(ns set-solver.core
  (:require [clojure.java.io :as io]
            [quil.core :as q]
            [quil.middleware :as m])
  (:import  [java.nio ByteBuffer ByteOrder]
            [org.opencv.core CvType Mat MatOfByte MatOfInt4 MatOfFloat MatOfPoint MatOfPoint2f Size TermCriteria Point Scalar]
            [org.opencv.imgproc Imgproc]
            [org.opencv.imgcodecs Imgcodecs]))

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
       (.contains r1 (.br r1))))

(defn rect-contained-in-any [r rs]
  (some #(rect-contains % r) rs))

;; given vec of [rect data] pairs, return list of {:data data :rect r :children c}
(defn nest-recs [pairs]
  (let [pairs (remove #(rect-contained-in-any (first %) (map first pairs))
                      pairs)]
    (for [[rect data] pairs
          :let [children (filter #(rect-contains rect (first %)) pairs)
                children (remove #(rect-contained-in-any (first %) (map first children)) children)]]
      {:data data
       :rect rect
       :children (nest-recs children)})))

(comment nest-recs
 (map vector
      (map #(Imgproc/boundingRect %) @c)
      @c))


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
    (doseq [[i h] (map vector
                       (range (.size contours))
                       (partition 4 (.toList hierarchy)))]
      (let [color (Scalar. (rand-int 255) (rand-int 255) (rand-int 255))
            rect (Imgproc/boundingRect (.get contours i))
            src (.submat img rect)
            dst (.submat drawing rect)
            ch (map vector contours (partition 4 (.toList hierarchy)))
            children (filter #(= i (last (second %))) ch)
            ]
                                        ;(.copyTo src dst)
        (Imgproc/rectangle drawing (.tl rect) (.br rect) color 2)
        (comment
          (doseq [[cc _] children
                  :let [r (Imgproc/boundingRect cc)]]
            (println r)
            (Imgproc/rectangle drawing (.tl r) (.br r) color 1)))
                                        ;(Imgproc/drawContours drawing contours i color 2 8 hierarchy 0 (Point.))
        ))
    (compare-and-set! h @h hierarchy)
    (compare-and-set! c @c contours)
    (comment (let [wat (Mat. height width CvType/CV_8UC3)]
               (Imgproc/cvtColor img-gray wat Imgproc/COLOR_GRAY2RGB 3)
               wat))
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
