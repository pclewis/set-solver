(ns set-solver.quil
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [round]]
            [quil.applet :as a]
            [quil.core :as q]
            [quil.middleware :as m]
            [set-solver.core :refer :all]
            [set-solver.util :refer [constrain enumerate]]
            [set-solver.reusable-buffer :refer [reusable]]
            [com.evocomputing.colors :as colors])
  (:import [java.nio ByteBuffer ByteOrder]
           [org.opencv.core Mat Size Rect Point CvType Scalar]
           [org.opencv.imgcodecs Imgcodecs]
           [org.opencv.imgproc Imgproc]
           [org.opencv.videoio VideoCapture]
           [java.lang.management ManagementFactory]
           [processing.core PImage]))


(defn ^PImage mat->p-img
  "Convert a Mat to a p-img"
  ([^Mat mat]
   (mat->p-img mat
               (condp = (.type mat)
                 CvType/CV_8U   Imgproc/COLOR_GRAY2RGBA
                 CvType/CV_8UC3 Imgproc/COLOR_RGB2RGBA
                 nil) ))
  ([^Mat mat conv]
   (let [n-pixels        (* (.width mat) (.height mat))
         ^bytes b-array  (reusable (byte-array (* 4 n-pixels)))
         ^ints i-array   (reusable (int-array n-pixels))
         ^PImage p-img   (reusable (q/create-image (.width mat) (.height mat) :rgb))]

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

(defn resize-keep-ratio
  "Resize an OpenCV Mat to fit in provided dimensions, preserving aspect ratio."
  [mat max-width max-height]
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
        (assoc state :image (reasonable-size (Imgcodecs/imread (:image-file state))))
        (assoc state :debug-canvas (Mat/zeros (.size (:image state)) CvType/CV_8U))))

(defn filter-query
  "Filter collection to contain only items that match query."
  [query coll]
  (let [query (.toLowerCase query)]
    (if (empty? query)
      coll
      (filter #(.contains (.toLowerCase %) query) coll))))

(defn load-next-image [state]
  (let [next-file (->> (filter-query (:file-query state) (:file-list state))
                       (partition-by #{(:image-file state)})
                       (remove #{(list (:image-file state))})
                       (last)
                       (first))]
    (load-image state (or next-file (first (:file-list state))))))

(defn load-prev-image [state]
  (let [prev-file (->> (filter-query (:file-query state) (:file-list state))
                       (take-while (complement #{(:image-file state)}) )
                       (last))]
    (load-image state (or prev-file (last (:file-list state))))))

(defn load-camera-image [state]
  (as-> state state
        (assoc state :camera (or (:camera state) (VideoCapture. 0)))
        (assoc state :image-file "CAMERA")
        (assoc state :image (reasonable-size (let [m (Mat.)]
                                               (.read (:camera state) m)
                                               m)))
        (assoc state :debug-canvas (Mat/zeros (.size (:image state)) CvType/CV_8U)))
  )

(defn key-pressed [state e]
  (println e)
  (if-let [field (:typing state)]
    (case (:key-code e)
      10 (assoc state :typing false)
      8  (update-in state [field] #(apply str (drop-last %)))
      47 (assoc state field "") ;; /
      27 (do (set! (.key (a/current-applet)) (char 0))
             (when (= :query field) (reset! (:debug state) nil))
             (assoc state :typing false field ""))
      (if (<= 32 (:key-code e) 128)
        (update-in state [field] #(str % (:raw-key e)))
        state))

    ;; not typing
    (case (:key e)
      :c (update-in state [:show-cards] not)
      :s (update-in state [:show-card-sets] not)
      :i (update-in state [:show-card-info] not)
      :q (update-in state [:query-filter] not)
      :j (update-in state [:top] #(+ 15 %))
      :k (update-in state [:top] #(- % 15))
      :l (update-in state [:left] #(+ 15 %))
      :h (update-in state [:left] #(- % 15))
      :C (load-camera-image state)
      :+ (update-in state [:zoom] #(* % 2))
      :- (update-in state [:zoom] #(/ % 2))
      :/ (assoc state :typing :query :query "")
      :f (assoc state :typing :file-query :file-query "")
      :down (load-next-image state)
      :up (load-prev-image state)
      :left (update-in state [:debug-offset] dec)
      :right (update-in state [:debug-offset] inc)
      :0 (assoc state :debug-offset 0)
      state)))

(defn mouse-dragged [state {:keys [x y p-x p-y button]}]
  (if-not (= button :left)
    state
    (-> state
        (update-in [:left]  #(+ (- p-x x) %))
        (update-in [:top] #(+ (- p-y y) %)))))

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
            :file-query ""
            :show-cards false
            :show-card-info false
            :query-filter true
            :debug (atom nil)
            :debug-offset 0
            :debug-avail (atom [])
            :debug-selected (atom [])
            :pid (re-find #"\d+" (.getName (ManagementFactory/getRuntimeMXBean)))})))



(defn debug [state text f]
  (let [query (.toLowerCase (:query state))
        offset (->> (:debug-avail state)
                    (deref)
                    (filter #(.contains (.toLowerCase %) query))
                    (count))]
    (when (or (not (:query-filter state))
            (.contains (.toLowerCase text) query))
      (swap! (:debug-avail state) conj text))

    (when (and (not-empty query)
               (nil? (deref (:debug state)))
               (= offset (:debug-offset state))
               (.contains (.toLowerCase text) query))
      (swap! (:debug-selected state) conj text)
      (let [canvas (:debug-canvas state)
            _ (.setTo canvas (Scalar. 0 0 0))
            result (f canvas)
            img (if (= (type result) org.opencv.core.Mat)
                  (.clone result)
                  canvas)]
        (reset! (:debug state) img)))))

(defn get-rss
  "Get the resident set size of pid when procfs is available, otherwise return nil."
  [pid]
  (let [file (clojure.java.io/as-file (str "/proc/" pid "/statm"))]
    (when (.exists file)
      (-> (java.io.FileReader. file)    ;; buffered reader doesn't always work on /proc
          (slurp)
          (clojure.string/split #"\s+") ;; size resident share text lib data dt
          (second)
          (Integer/parseInt)
          (* 4096)))))                  ;; HACK: assuming system page size is 4096

(defn add-sets
  [cards sets]
  (let [i-sets (enumerate sets)]
    (map (fn [card]
           (assoc card :in-sets
                  (map (comp inc first)
                       (filter (fn [[i s]] (.contains s card))
                              i-sets))))
         cards)))

(defn update [state]
  (reset! (:debug-avail state) [])
  (reset! (:debug-selected state) [])
  (reset! (:debug state) nil)
  (as-> state state
        (assoc state :cards (find-cards-intensity (:image state) (partial debug state)))
        (assoc state :card-props (map #(identify-card (:image state) %) (:cards state)))
        (assoc state :rss (get-rss (:pid state)))
        (assoc state :card-props (add-sets (:card-props state) (find-sets (:card-props state))))
        (assoc state :sets (find-sets (:card-props state)))))

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
  (q/text (str (select-keys state [:left :top :zoom :query :show-cards :query-filter :debug-offset :file-query])) 15 15)
  (q/text (str (count (:sets state)) " sets") 15 30)
  (q/text (str (round (q/current-frame-rate)) "/" (q/target-frame-rate)) 15 45)

  (let [debug-avail (deref (:debug-avail state))
        debug-avail (if (> (count debug-avail) 47)
                      (drop (max 0 (- (:debug-offset state) 15))
                            debug-avail)
                      debug-avail)]
    (doseq [[i line] (enumerate debug-avail)
            :let [visible (some #{line} (deref (:debug-selected state)))]]
      (if visible
        (q/fill 0 255 0)
        (q/fill 255))
      (q/text line 15 (+ 60 (* i 15)))))

  (doseq [[i line] (enumerate (filter-query (:file-query state) (:file-list state)))
          :let [visible (= line (:image-file state))]]
    (q/fill 0 0 0 192)
    (q/rect (- width 100) (* i 15) 100 15)
    (if visible
      (q/fill 0 255 0)
      (q/fill 255))
    (q/text (.replace ^String line "resources/" "") (- width 100) (+ 15 (* i 15))))

  (q/fill 0 0 0 192)
  (q/rect (- width 200) (- height 30) 200 30)
  (q/fill 255)
  (let [rt (Runtime/getRuntime)]
    (q/text (format "Java: %.1f MB\nRSS:  %s MB"
                    (float (/ (.totalMemory rt) 1024 1024))
                    (if-let [rss (:rss state)]
                      (format "%.1f" (float (/ rss 1024 1024)))
                      "unk"))
            (- width 100) (- height 15)))

  (q/fill 255)
  (when (:show-cards state)
    (doseq [c (:card-props state)
            :let [card-img (mat->p-img (:image c))
                  bb ^Rect (:bb c)]]

      (q/image card-img
               (- (* (:zoom state) (-> bb .tl .x))
                  (:left state))
               (- (* (:zoom state) (-> bb .tl .y))
                  (:top state))
               (* (.width card-img) 0.50 (:zoom state))
               (* (.height card-img) 0.50 (:zoom state)))))

  (when (:show-card-info state)
    ;(clojure.pprint/pprint (:card-props2 state))
    (doseq [c (:card-props state)
            :let [bb ^Rect (:bb c)]]
      (q/fill 0 0 0 192)
      (q/rect (- (* (:zoom state) (-> bb .tl .x))
                 (:left state) 5)
              (- (* (:zoom state) (-> bb .tl .y))
                 (:top state) 20)
              120 85)
      (q/fill 255)
      (q/text (->> (select-keys c [:count :fill :color :shape])
                   (map (juxt key val))
                   (clojure.string/join "\n"))
              (- (* (:zoom state) (-> bb .tl .x))
                 (:left state))
              (- (* (:zoom state) (-> bb .tl .y))
                 (:top state)))))

  (when (:show-card-sets state)
    (doseq [c (:card-props state)
            :let [bb ^Rect (:bb c)]]
      (q/fill 0 0 0 192)
      (q/rect (- (* (:zoom state) (-> bb .tl .x))
                 (:left state) 5)
              (- (* (:zoom state) (-> bb .tl .y))
                 (:top state)
                 -67)
              120 35)
      (let [size (if (< (count (:sets state)) 6) 30 10)]
        (q/text-size size)

        (doseq [i (:in-sets c)]
          (apply q/fill (colors/hsl-to-rgb (* i 30) 50 50))
          (q/text (str i)
                  (- (* (:zoom state) (-> bb .tl .x))
                     (:left state)
                     (* (dec i) (- size)))
                  (- (* (:zoom state) (-> bb .tl .y))
                     (:top state)
                     -95)
                  )))
      (q/text-size 12))))

(q/defsketch hi
  :title "hi"
  :size [width height]
  :setup setup
  :update update
  :draw draw
  :key-pressed key-pressed
  :mouse-dragged mouse-dragged
  :features [:keep-on-top]
  :middleware [m/fun-mode])
