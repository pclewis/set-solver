(ns set-solver.match-shapes
  (:require [set-solver.util :refer [sgn]]))

(def ^:private eps 1.e-5)

(defn- match-shapes [ma mb map-fn reduce-fn]
  (let [map-res (keep (fn [[ma mb]]
                        (let [ama (Math/abs ma)
                              amb (Math/abs mb)
                              sma (sgn ma)
                              smb (sgn mb)]
                          (when (and (> ama eps)
                                     (> amb eps))
                            (map-fn ama amb sma smb))))
                      (map vector ma mb))]
    (if (empty? map-res) 999
      (reduce reduce-fn map-res))))


;;       ama = 1. / (sma * log10( ama ));
;;       amb = 1. / (smb * log10( amb ));
;;       result += fabs( -ama + amb );
(defn- match-shapes-i1* [ama amb sma smb]
  (let [ama (Math/abs (/ 1 (* sma (Math/log10 ama))))
        amb (Math/abs (/ 1 (* smb (Math/log10 amb))))]
    (Math/abs (+ (- ama) amb))))

(defn match-shapes-i1 [ma mb]
  (match-shapes ma mb match-shapes-i1* +))

;;       ama = sma * log10( ama );
;;       amb = smb * log10( amb );
;;       result += fabs( -ama + amb );
(defn- match-shapes-i2* [ama amb sma smb]
  (let [ama (Math/abs (* sma (Math/log10 ama)))
        amb (Math/abs (* smb (Math/log10 amb)))]
    (Math/abs (+ (- ama) amb))))

(defn match-shapes-i2 [ma mb]
  (match-shapes ma mb match-shapes-i2* +))

;;       ama = sma * log10( ama );
;;       amb = smb * log10( amb );
;;       mmm = fabs( (ama - amb) / ama );
;;       if( result < mmm )
;;           result = mmm;
(defn- match-shapes-i3* [ama amb sma smb]
  (let [ama (* sma (Math/log10 ama))
        amb (* smb (Math/log10 amb))]
    (Math/abs (/ (- ama amb) ama))))

(defn match-shapes-i3 [ma mb]
  (match-shapes ma mb match-shapes-i3* max))


;; double cv::matchShapes(InputArray contour1, InputArray contour2, int method, double)
;; {
;;     double ma[7], mb[7];
;;     int i, sma, smb;
;;     double eps = 1.e-5;
;;     double mmm;
;;     double result = 0;

;;     HuMoments( moments(contour1), ma );
;;     HuMoments( moments(contour2), mb );

;;     switch (method)
;;     {
;;     case 1:
;;         for( i = 0; i < 7; i++ )
;;         {
;;             double ama = fabs( ma[i] );
;;             double amb = fabs( mb[i] );

;;             if( ma[i] > 0 )
;;                 sma = 1;
;;             else if( ma[i] < 0 )
;;                 sma = -1;
;;             else
;;                 sma = 0;
;;             if( mb[i] > 0 )
;;                 smb = 1;
;;             else if( mb[i] < 0 )
;;                 smb = -1;
;;             else
;;                 smb = 0;

;;             if( ama > eps && amb > eps )
;;             {
;;                 ama = 1. / (sma * log10( ama ));
;;                 amb = 1. / (smb * log10( amb ));
;;                 result += fabs( -ama + amb );
;;             }
;;         }
;;         break;

;;     case 2:
;;         for( i = 0; i < 7; i++ )
;;         {
;;             double ama = fabs( ma[i] );
;;             double amb = fabs( mb[i] );

;;             if( ma[i] > 0 )
;;                 sma = 1;
;;             else if( ma[i] < 0 )
;;                 sma = -1;
;;             else
;;                 sma = 0;
;;             if( mb[i] > 0 )
;;                 smb = 1;
;;             else if( mb[i] < 0 )
;;                 smb = -1;
;;             else
;;                 smb = 0;

;;             if( ama > eps && amb > eps )
;;             {
;;                 ama = sma * log10( ama );
;;                 amb = smb * log10( amb );
;;                 result += fabs( -ama + amb );
;;             }
;;         }
;;         break;

;;     case 3:
;;         for( i = 0; i < 7; i++ )
;;         {
;;             double ama = fabs( ma[i] );
;;             double amb = fabs( mb[i] );

;;             if( ma[i] > 0 )
;;                 sma = 1;
;;             else if( ma[i] < 0 )
;;                 sma = -1;
;;             else
;;                 sma = 0;
;;             if( mb[i] > 0 )
;;                 smb = 1;
;;             else if( mb[i] < 0 )
;;                 smb = -1;
;;             else
;;                 smb = 0;

;;             if( ama > eps && amb > eps )
;;             {
;;                 ama = sma * log10( ama );
;;                 amb = smb * log10( amb );
;;                 mmm = fabs( (ama - amb) / ama );
;;                 if( result < mmm )
;;                     result = mmm;
;;             }
;;         }
;;         break;
;;     default:
;;         CV_Error( CV_StsBadArg, "Unknown comparison method" );
;;     }

;;     return result;
;; }
