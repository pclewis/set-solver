(defproject set-solver "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [quil "2.2.6"]
                 [opencv/opencv "3.0.0"]
                 [opencv/opencv-native "3.0.0"]
                 [org.clojure/core.cache "0.6.3"]
                ; [com.evocomputing/colors "1.0.0-SNAPSHOT" :exclusions [org.clojure/clojure-contrib]]
                 [org.clojars.brunchboy/colors "1.0.2-SNAPSHOT"]
                  ;; required by native-vector
                 [org.clojure/math.numeric-tower "0.0.4"]]

  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)])
