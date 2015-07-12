(ns set-solver.perspective-ratio)

;; Adapted from HugoRune's code at
;; https://stackoverflow.com/questions/1194352/proportions-of-a-perspective-deformed-rectangle
;;
;; License: GPLv2 or later

(defn square [n] (* n n))

;; Point order:
;; m3---m4
;; |     |
;; m1---m2
;;
;; Input order:
;; m2---m1
;; |     |
;; m3---m4

(defn perspective-ratio
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
      (Math/sqrt (/ (+ (square (- m2y m1y))
                       (square (- m2x m1x)))
                    (+ (square (- m3y m1y))
                       (square (- m3x m1x)))))
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
        (Math/sqrt (/ (+ (square (dec k2))
                         (/ (square (- (* k2 m2y) m1y))
                            f-squared)
                         (/ (square (- (* k2 m2x) m1x))
                            f-squared))
                      (+ (square (dec k3))
                         (/ (square (- (* k3 m3y) m1y))
                            f-squared)
                         (/ (square (- (* k3 m3x) m1x))
                            f-squared))))))))


;; // in case it matters: licensed under GPLv2 or later
;; // legend:
;; // sqr(x)  = x*x
;; // sqrt(x) = square root of x

;; // let m1x,m1y ... m4x,m4y be the (x,y) pixel coordinates
;; // of the 4 corners of the detected quadrangle
;; // i.e. (m1x, m1y) are the cordinates of the first corner,
;; // (m2x, m2y) of the second corner and so on.
;; // let u0, v0 be the pixel coordinates of the principal point of the image
;; // for a normal camera this will be the center of the image,
;; // i.e. u0=IMAGEWIDTH/2; v0 =IMAGEHEIGHT/2
;; // This assumption does not hold if the image has been cropped asymmetrically

;; // first, transform the image so the principal point is at (0,0)
;; // this makes the following equations much easier
;; m1x = m1x - u0;
;; m1y = m1y - v0;
;; m2x = m2x - u0;
;; m2y = m2y - v0;
;; m3x = m3x - u0;
;; m3y = m3y - v0;
;; m4x = m4x - u0;
;; m4y = m4y - v0;


;; // temporary variables k2, k3
;; double k2 = ((m1y - m4y)*m3x - (m1x - m4x)*m3y + m1x*m4y - m1y*m4x) /
;;             ((m2y - m4y)*m3x - (m2x - m4x)*m3y + m2x*m4y - m2y*m4x) ;

;; double k3 = ((m1y - m4y)*m2x - (m1x - m4x)*m2y + m1x*m4y - m1y*m4x) /
;;             ((m3y - m4y)*m2x - (m3x - m4x)*m2y + m3x*m4y - m3y*m4x) ;

;; // f_squared is the focal length of the camera, squared
;; // if k2==1 OR k3==1 then this equation is not solvable
;; // if the focal length is known, then this equation is not needed
;; // in that case assign f_squared= sqr(focal_length)
;; double f_squared =
;;     -((k3*m3y - m1y)*(k2*m2y - m1y) + (k3*m3x - m1x)*(k2*m2x - m1x)) /
;;                       ((k3 - 1)*(k2 - 1)) ;

;; //The width/height ratio of the original rectangle
;; double whRatio = sqrt(
;;     (sqr(k2 - 1) + sqr(k2*m2y - m1y)/f_squared + sqr(k2*m2x - m1x)/f_squared) /
;;     (sqr(k3 - 1) + sqr(k3*m3y - m1y)/f_squared + sqr(k3*m3x - m1x)/f_squared)
;; ) ;

;; // if k2==1 AND k3==1, then the focal length equation is not solvable
;; // but the focal length is not needed to calculate the ratio.
;; // I am still trying to figure out under which circumstances k2 and k3 become 1
;; // but it seems to be when the rectangle is not distorted by perspective,
;; // i.e. viewed straight on. Then the equation is obvious:
;; if (k2==1 && k3==1) whRatio = sqrt(
;;     (sqr(m2y-m1y) + sqr(m2x-m1x)) /
;;     (sqr(m3y-m1y) + sqr(m3x-m1x))


;; // After testing, I found that the above equations
;; // actually give the height/width ratio of the rectangle,
;; // not the width/height ratio.
;; // If someone can find the error that caused this,
;; // I would be most grateful.
;; // until then:
;; whRatio = 1/whRatio;
