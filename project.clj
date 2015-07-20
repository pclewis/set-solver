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
                 [org.clojure/math.combinatorics "0.1.1"]
                 ;; required by native-vector
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [compojure "1.3.4"]
                 [http-kit "2.1.18"]
                 [net.mikera/core.matrix "0.36.1"]
                 [net.mikera/vectorz-clj "0.30.1"]
                 ;; required for ring.middleware.multipart-params, normally included with ex jetty
                 [javax.servlet/servlet-api "2.5"]
                 [org.clojure/data.json "0.2.6"]
                 [prismatic/plumbing "0.4.4"]]
  :global-vars {*warn-on-reflection* true}

  :injections [(clojure.lang.RT/loadLibrary org.opencv.core.Core/NATIVE_LIBRARY_NAME)])
