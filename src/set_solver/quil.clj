(ns set-solver.quil
  (:require [set-solver.util :refer [constrain]]
            [set-solver.core :refer :all]
            [quil.core :as q]
            [quil.middleware :as m]
            [clojure.math.numeric-tower :refer [round]])
  (:import [org.opencv.core Mat Size Rect Point CvType]
           [org.opencv.imgproc Imgproc]
           [org.opencv.imgcodecs Imgcodecs]
           [java.nio ByteBuffer ByteOrder]))

(defn mat->p-img [{:keys [canny in-mat tmp-mat out-mat b-array i-array p-img top left zoom]}]
  ;
  (let [rm (Mat.)
        _(Imgproc/resize canny rm (Size.) zoom zoom Imgproc/INTER_AREA)
        rw (min (.width out-mat) (.width rm))
        rh (min (.height out-mat) (.height rm))
        rs (Size. rw rh)
        rect (Rect. (Point. (constrain left 0 (- (.width rm) (.width out-mat)))
                            (constrain top 0 (- (.height rm) (.height out-mat))))
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

(defn convert-mat [img]
  (let [out-mat (Mat.)
        n-pixels (* (.width img) (.height img))
        b-array (byte-array (* 4 n-pixels))
        i-array (int-array n-pixels)
        p-img   (q/create-image (.width img) (.height img) :rgb)]
    (Imgproc/cvtColor img out-mat Imgproc/COLOR_RGB2RGBA 4)
    (.get out-mat 0 0 b-array)
    (-> (ByteBuffer/wrap b-array)
        (.order ByteOrder/LITTLE_ENDIAN)
        (.asIntBuffer)
        (.get i-array))
    (.loadPixels p-img)
    (set! (.pixels p-img) (aclone i-array))
    (.updatePixels p-img)
    p-img))

(defn key-pressed [state e]
  (case (:key e)
    :j (update-in state [:mat-converter :top] #(+ 15 %))
    :k (update-in state [:mat-converter :top] #(- % 15))
    :l (update-in state [:mat-converter :left] #(+ 15 %))
    :h (update-in state [:mat-converter :left] #(- % 15))
    :+ (update-in state [:mat-converter :zoom] #(* % 2))
    :- (update-in state [:mat-converter :zoom] #(/ % 2))
    state))

(defn setup2 []
  (let [image (Imgcodecs/imread "resources/overhead.jpg")
        cards (find-cards image)
        card-props (map #(identify-card image %) cards)
        state {:image image
               :card-props card-props
               :mat-converter {:canny   image
                               :tmp-mat (Mat. height width CvType/CV_8UC3)
                               :out-mat (Mat. height width CvType/CV_8UC4)
                               :b-array (byte-array (* 4 n-pixels))
                               :i-array (int-array n-pixels)
                               :p-img   (q/create-image width height :rgb)
                               :top 0
                               :left 0
                               :zoom 1.0}}]
    state) )

(defn update2 [state]
  (assoc-in state [:mat-converter :p-img] (mat->p-img (:mat-converter state)))  )

(defn draw2 [state]
  (q/background 0)
  (q/image (:p-img (:mat-converter state)) 1 1)
  (q/fill 0 0 0 192)
  (q/rect 0 0 300 60)
  (q/fill 255)
  (q/text (str (select-keys (:mat-converter state) [:left :top :zoom])) 15 15)
  (q/text (str (round (q/current-frame-rate)) "/" (q/target-frame-rate)) 15 45)
  (doseq [c (:card-props state)
          :let [mc (:mat-converter state)]]

    (if (< 3 (:count c))
      (q/image (convert-mat (:image c))
               (- (* (:zoom mc) (-> c :bb .tl .x))
                  (:left mc))
               (- (* (:zoom mc) (-> c :bb .tl .y))
                  (:top mc))
               ))
    (q/text (str (select-keys c [:count :fill :color :shape]))
          (- (* (:zoom mc) (-> c :bb .tl .x))
               (:left mc))
           (- (* (:zoom mc) (-> c :bb .tl .y))
               (:top mc))
            )))

(q/defsketch hi
  :title "hi"
  :size [width height]
  :setup setup2
  :update update2
  :draw draw2
  :key-pressed key-pressed
  :features [:keep-on-top]
  :middleware [m/fun-mode])
