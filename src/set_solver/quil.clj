(ns set-solver.quil
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [round]]
            [quil.applet :as a]
            [quil.core :as q]
            [quil.middleware :as m]
            [set-solver.core :refer :all]
            [set-solver.util :refer [constrain]])
  (:import [java.nio ByteBuffer ByteOrder]
           [org.opencv.core Mat Size Rect Point CvType]
           [org.opencv.imgcodecs Imgcodecs]
           [org.opencv.imgproc Imgproc]))


(defn mat->p-img
  "Convert a Mat to a p-img"
  ([mat]
   (mat->p-img mat
               (condp = (.type mat)
                 CvType/CV_8U   Imgproc/COLOR_GRAY2RGBA
                 CvType/CV_8UC3 Imgproc/COLOR_RGB2RGBA
                 nil) ))
  ([mat conv]
   (let [n-pixels (* (.width mat) (.height mat))
         b-array  (byte-array (* 4 n-pixels))
         i-array  (int-array n-pixels)
         p-img    (q/create-image (.width mat) (.height mat) :rgb)]

     ;; convert color if necessary
     (if (nil? conv)
       (.get mat 0 0 b-array)
       (let [tmp-mat (Mat. (.size mat) CvType/CV_8UC4)]
         (Imgproc/cvtColor mat tmp-mat conv 4)
         (.get tmp-mat 0 0 b-array)))

     ;; convert byte array to integer array
     (-> (ByteBuffer/wrap b-array)
         (.order ByteOrder/LITTLE_ENDIAN)
         (.asIntBuffer)
         (.get i-array))

     ;; load pixels into p-img
     (.loadPixels p-img)
     (set! (.pixels p-img) i-array)
     (.updatePixels p-img)
     p-img)))

(defn resize-keep-ratio [mat max-width max-height]
  (let [origAR (/ (.width mat) (.height mat))
        newAR (/ max-width max-height)
        ratio (if (> origAR newAR)
                (/ max-width (.width mat))
                (/ max-height (.height mat)))
        new-size (Size. (* ratio (.width mat)) (* ratio (.height mat)))
        new-mat (Mat/zeros new-size (.type mat))]
    (Imgproc/resize mat new-mat new-size)
    new-mat))

(defn reasonable-size [mat]
  (if (or (< 1200 (.width mat))
          (< 1000 (.height mat)))
    (resize-keep-ratio mat 1200 1000)
    mat))

(defn load-image [state file-name]
  (as-> state state
        (assoc state :image-file file-name)
        (assoc state :image (reasonable-size (Imgcodecs/imread (:image-file state))))))

(defn load-next-image [state]
  (load-image state
              (let [next-file (->> (:file-list state)
                                   (partition-by #{(:image-file state)})
                                   (filter #(< 1 (count %)))
                                   (last)
                                   (first))]
                (or next-file (first (:file-list state))))))

(defn load-prev-image [state]
  (load-image state
              (let [prev-file (->> (:file-list state)
                                   (take-while (complement #{(:image-file state)}) )
                                   (last))]
                (or prev-file (last (:file-list state))))))

(defn key-pressed [state e]
  (println e)
  (if (:typing state)
    (case (:key-code e)
      10 (assoc state :typing false)
      8  (update-in state [:query] #(apply str (drop-last %)))
      27 (do (set! (.key (a/current-applet)) (char 0))
             (reset! (:debug state) nil)
             (assoc state :typing false :query ""))
      (if (<= 32 (:key-code e) 128)
        (update-in state [:query] #(str % (:raw-key e)))
        state))

    ;; not typing
    (case (:key e)
      :c (update-in state [:show-cards] not)
      :j (update-in state [:top] #(+ 15 %))
      :k (update-in state [:top] #(- % 15))
      :l (update-in state [:left] #(+ 15 %))
      :h (update-in state [:left] #(- % 15))
      :+ (update-in state [:zoom] #(* % 2))
      :- (update-in state [:zoom] #(/ % 2))
      :/ (assoc state :typing true :query "")
      :down (load-next-image state)
      :up (load-prev-image state)
      state)))

(defn setup []
  (let [file-list (->> (io/file "resources/")
                       (file-seq)
                       (map str)
                       (filter #(re-matches #"resources/[^/]+\.(jpg|png|jpeg)" %))
                       (sort))]
    (merge (load-image {:file-list file-list} (first file-list))
           {:zoom 1.0
            :left 0
            :top 0
            :query ""
            :show-cards false
            :debug (atom nil)
            :debug-avail (atom [])
            :debug-selected (atom [])})))

(defn debug [state f text]
  (let [query (.toLowerCase (:query state))]
    (swap! (:debug-avail state) conj text)
    (when (and (not-empty query)
               (.contains (.toLowerCase text) query))
      (swap! (:debug-selected state) conj text)
      (reset! (:debug state) (.clone (f))))))

(defn update [state]
  (reset! (:debug-avail state) [])
  (reset! (:debug-selected state) [])
  (as-> state state
        (assoc state :cards (find-cards-debug (:image state) (partial debug state)))
        (assoc state :card-props (map #(identify-card (:image state) %) (:cards state)))))

(defn draw [state]
  (q/background 0)

  (let [img (mat->p-img (or (deref (:debug state)) (:image state)))]
    (q/image img
             (- (:left state)) (- (:top state))
             (* (:zoom state) (.width img))
             (* (:zoom state) (.height img))))
  (q/fill 0 0 0 192)
  (q/rect 0 0 300 60)
  (q/fill 255)
  (q/text (str (select-keys state [:left :top :zoom :query])) 15 15)
  (q/text (str (round (q/current-frame-rate)) "/" (q/target-frame-rate)) 15 45)

  (doseq [[line i] (map vector (deref (:debug-avail state)) (range))
          :let [visible (some #{line} (deref (:debug-selected state)))]]
    (if visible
      (q/fill 0 255 0)
      (q/fill 255))
    (q/text line 15 (+ 60 (* i 15))))

  (doseq [[line i] (map vector (:file-list state) (range))
          :let [visible (= line (:image-file state))]]
    (q/fill 0 0 0 192)
    (q/rect (- width 100) (* i 15) 100 15)
    (if visible
      (q/fill 0 255 0)
      (q/fill 255))
    (q/text (.replace line "resources/" "") (- width 100) (+ 15 (* i 15))))

  (q/fill 255)
  (when (:show-cards state)
    (doseq [c (:card-props state)
            :let [card-img (mat->p-img (:image c))]]

      (q/image card-img
               (- (* (:zoom state) (-> c :bb .tl .x))
                  (:left state))
               (- (* (:zoom state) (-> c :bb .tl .y))
                  (:top state))
               (* (.width card-img) 0.5)
               (* (.height card-img) 0.5)
               )
      (q/text (str (select-keys c [:count :fill :color :shape]))
              (- (* (:zoom state) (-> c :bb .tl .x))
                 (:left state))
              (- (* (:zoom state) (-> c :bb .tl .y))
                 (:top state))
              ))))

(q/defsketch hi
  :title "hi"
  :size [width height]
  :setup setup
  :update update
  :draw draw
  :key-pressed key-pressed
  :features [:keep-on-top]
  :middleware [m/fun-mode])
