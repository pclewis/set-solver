(ns set-solver.perspective-ratio
  "Methods to estimate a camera's focal length and rectangle's aspect ratio.
  Point order in calculations:
  m3---m4
  |     |
  m1---m2

  Input order:
  m2---m1
  |     |
  m3---m4

  u0 and v0 are the focal point of the image, usually width/2 and height/2.

  s is the pixel ratio, which should be 1.

  Based on http://research.microsoft.com/en-us/um/people/zhang/papers/tr03-39.pdf"
  (:require [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :as matrix])
  (:import [org.opencv.core Core MatOfDouble Scalar]))

(set-current-implementation :vectorz)

(defn calculate-matrices
  "Calculate matrices used by estimate-focal-length and rectangle-ratio."
  [m1x m1y m2x m2y m3x m3y m4x m4y u0 v0]
  (let [;; "Augment" vectors, see bottom of page 11
        m1 (matrix [m1x m1y 1.0])
        m2 (matrix [m2x m2y 1.0])
        m3 (matrix [m3x m3y 1.0])
        m4 (matrix [m4x m4y 1.0])

        ;; equation 11
        k2 (/ (-> m1 (cross m4) (dot m3))
              (-> m2 (cross m4) (dot m3)))

        ;; equation 12
        k3 (/ (-> m1 (cross m4) (dot m2))
              (-> m3 (cross m4) (dot m2)))

        ;; equation 14
        n2 (-> (mmul m2 k2)
               (matrix/- m1))

        ;; equation 16
        n3 (-> (mmul m3 k3)
               (matrix/- m1))]
    [m1 m2 m3 m4 n2 n3]))

(defn estimate-focal-length
  "Estimate focal length of image given the corners of a rectangle in perspective.
   Returns nil if incalculable."
  ([m4x m4y m3x m3y m1x m1y m2x m2y u0 v0]
   (apply estimate-focal-length
          (concat (calculate-matrices m1x m1y m2x m2y m3x m3y m4x m4y u0 v0)
                  [u0 v0 1])))

  ([m1 m2 m3 m4 n2 n3 u0 v0 s]
   (let [;; equation 21
         [n21 n22 n23] (seq n2)
         [n31 n32 n33] (seq n3)
         fsq1-divisor (* n23 n33 (* s s))]
     (when-not (zero? fsq1-divisor)
       (let [fsq1 (/ 1 fsq1-divisor)
             fsq2 (+ (- (* n21 n31)
                        (* (+ (* n21 n33)
                              (* n23 n31))
                           u0))
                     (* n23 n33 (* u0 u0)))

             fsq3 (+ (- (* n22 n32)
                        (* (+ (* n22 n33)
                              (* n23 n32))
                           v0))
                     (* n23 n33 (* v0 v0)))

             f-squared (* (- fsq1) (+ (* fsq2 (* s s)) fsq3))]
         ;; In practice, f-squared is often negative, but the sqrt of
         ;; the abs behaves reasonably.
         (Math/sqrt (Math/abs f-squared)))))) )

(defn rectangle-aspect-ratio
  "Estimate the aspect ratio of a rectangle given the corners."
  ([m4x m4y m3x m3y m1x m1y m2x m2y u0 v0]
   (rectangle-aspect-ratio m4x m4y m3x m3y m1x m1y m2x m2y u0 v0 nil))

  ([m4x m4y m3x m3y m1x m1y m2x m2y u0 v0 focal-length]
   (rectangle-aspect-ratio
    (calculate-matrices m1x m1y m2x m2y m3x m3y m4x m4y u0 v0)
    u0 v0 1 focal-length))

  ([[m1 m2 m3 m4 n2 n3] u0 v0 s focal-length]
   (let [[n21 n22 n23] (seq n2)
         [n31 n32 n33] (seq n3)

         ;; 480/100 is the focal length reported in the EXIF headers of test pictures
         ;; but I don't think mm is the correct unit here.
         f (or focal-length
               (estimate-focal-length m1 m2 m3 m4 n2 n3 u0 v0 s)
               (/ 480 100))

         ;; see equation 1
         a (matrix [[f   0        u0]
                    [0   (* s f)  v0]
                    [0   0        1]])

         a-1 (inverse a)

         ;; equation 20
         whr1 (-> (mmul (transpose n2) (transpose a-1))
                  (mmul a-1)
                  (mmul n2))

         whr2 (-> (mmul (transpose n3) (transpose a-1))
                  (mmul a-1)
                  (mmul n3))

         whr-squared (/ (.get whr1) (.get whr2))
         whr (Math/sqrt whr-squared)]
     whr)))

;; Adapted from HugoRune's code at
;; https://stackoverflow.com/questions/1194352/proportions-of-a-perspective-deformed-rectangle
;;
;; License: GPLv2 or later

(defn sqr [n] (* n n))

(defn rectangle-aspect-ratio-hugorune
  "Alternate implementation of rectangle-aspect-ratio based on HugeRune's code at
  https://stackoverflow.com/questions/1194352/proportions-of-a-perspective-deformed-rectangle"
  [m4x m4y m3x m3y m1x m1y m2x m2y u0 v0]
  (let [[m1x m2x m3x m4x] (map #(- (double %) u0) [m1x m2x m3x m4x])
        [m1y m2y m3y m4y] (map #(- (double %) v0) [m1y m2y m3y m4y])

        ;; double k2 = ((m1y - m4y)*m3x - (m1x - m4x)*m3y + m1x*m4y - m1y*m4x) /
        ;;             ((m2y - m4y)*m3x - (m2x - m4x)*m3y + m2x*m4y - m2y*m4x) ;
        k2 (/ (+ (- (* (- m1y m4y) m3x)
                    (* (- m1x m4x) m3y))
                 (- (* m1x m4y)
                    (* m1y m4x)))
              (+ (- (* (- m2y m4y) m3x)
                    (* (- m2x m4x) m3y))
                 (- (* m2x m4y)
                    (* m2y m4x))))

        ;; double k3 = ((m1y - m4y)*m2x - (m1x - m4x)*m2y + m1x*m4y - m1y*m4x) /
        ;;             ((m3y - m4y)*m2x - (m3x - m4x)*m2y + m3x*m4y - m3y*m4x) ;
        k3 (/ (+ (- (* (- m1y m4y) m2x)
                    (* (- m1x m4x) m2y))
                 (- (* m1x m4y)
                    (* m1y m4x)))
              (+ (- (* (- m3y m4y) m2x)
                    (* (- m3x m4x) m2y))
                 (- (* m3x m4y)
                    (* m3y m4x))))]

    (if (< (Math/abs (* (dec k3) (dec k2))) 0.001)
      ;; if (k2==1 && k3==1) whRatio = sqrt(
      ;;     (sqr(m2y-m1y) + sqr(m2x-m1x)) /
      ;;     (sqr(m3y-m1y) + sqr(m3x-m1x))
      (Math/sqrt (/ (+ (sqr (- m2y m1y))
                       (sqr (- m2x m1x)))
                    (+ (sqr (- m3y m1y))
                       (sqr (- m3x m1x)))))
      ;; double f_squared =
      ;;     -((k3*m3y - m1y)*(k2*m2y - m1y) + (k3*m3x - m1x)*(k2*m2x - m1x)) /
      ;;                       ((k3 - 1)*(k2 - 1)) ;
      (let [f-squared (/ (- (+ (* (- (* k3 m3y) m1y)
                                  (- (* k2 m2y) m1y))
                               (* (- (* k3 m3x) m1x)
                                  (- (* k2 m2x) m1x))))
                         (* (dec k3) (dec k2)))]
        ;; double whRatio = sqrt(
        ;;     (sqr(k2 - 1) + sqr(k2*m2y - m1y)/f_squared + sqr(k2*m2x - m1x)/f_squared) /
        ;;     (sqr(k3 - 1) + sqr(k3*m3y - m1y)/f_squared + sqr(k3*m3x - m1x)/f_squared)
        ;; ) ;
        (Math/sqrt (/ (+ (sqr (dec k2))
                         (/ (sqr (- (* k2 m2y) m1y))
                            f-squared)
                         (/ (sqr (- (* k2 m2x) m1x))
                            f-squared))
                      (+ (sqr (dec k3))
                         (/ (sqr (- (* k3 m3y) m1y))
                            f-squared)
                         (/ (sqr (- (* k3 m3x) m1x))
                            f-squared))))))))
