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

(defn load-image [] (Imgcodecs/imread "resources/overhead.jpg"))

(defn color-name [hue]
  (cond (< hue 200) :green
        (< hue 275) :red
        :else       :purple))

(defn scalar-color-name [color]
  (let [[h _ _] (scalar-to-hsl color)]
    (color-name h)))

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
      (Imgproc/threshold target-gray target-edges m 255 Imgproc/THRESH_BINARY_INV)
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

          _ (let [m (-> (Core/mean target-gray) .val first (* 0.80))]
              (Imgproc/threshold target-gray target-edges m 255 Imgproc/THRESH_BINARY_INV)
              (Imgproc/rectangle target-edges
                                 (Point. 0 0)
                                 (.br (Rect. (Point. 0 0) (.size target)))
                                 (Scalar. 0 0 0) 50)
              (Imgproc/cvtColor target-edges target Imgproc/COLOR_GRAY2BGR))
          ]
      {:count (count card-contours)
       :shape sname
       :color cname
       :fill fill
       :contour c
       :bb br
       :image target})))


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
         (filter is-rectangle)
         ;; eliminate overlapping contours
         (reduce (fn [cs c]
                   (if (some #(rect-close-enough? (Imgproc/boundingRect %) (Imgproc/boundingRect c)) cs)
                     cs
                     (conj cs c)))
                 []))))

(defn mat->p-img [{:keys [canny in-mat tmp-mat out-mat b-array i-array p-img top left zoom]}]
  ;
  (let [rm (Mat.)
        _(Imgproc/resize canny rm (Size.) zoom zoom Imgproc/INTER_AREA)
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
  (q/frame-rate frame-rate)
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
  (println "what")
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
            ))
  (q/fill (:color state) 255 255))

(q/defsketch hi
  :title "hi"
  :size [width height]
  :setup setup2
  :update update2
  :draw draw2
  :key-pressed key-pressed
  :features [:keep-on-top]
  :middleware [m/fun-mode])
