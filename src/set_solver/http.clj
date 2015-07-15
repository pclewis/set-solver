(ns set-solver.http
  (:require [compojure.core :refer :all]
            [compojure.route :as route]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.multipart-params.byte-array :refer [byte-array-store]]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as r]
            [org.httpkit.server :as hk]
            [clojure.data.json :as json]
            [set-solver.core :as solver]
            [set-solver.quil :refer [reasonable-size]])
  (:import [java.io ByteArrayInputStream]
           [org.opencv.core MatOfByte]
           [org.opencv.imgcodecs Imgcodecs]))

(defn echo [params]
  (let [img (Imgcodecs/imdecode (MatOfByte. (-> params :file :bytes)) Imgcodecs/CV_LOAD_IMAGE_COLOR)
        out (MatOfByte.)]
    (Imgcodecs/imencode ".jpg" img out)
    (-> (r/response (ByteArrayInputStream. (.toArray out)))
        (r/content-type "image/jpg"))))


(defn- mapify-bb [k v]
  (if (= k :bb)
    {:x (.x v) :y (.y v) :width (.width v) :height (.height v)}
    v))

(defn solve [params]
  (let [img (reasonable-size (Imgcodecs/imdecode (MatOfByte. (-> params :file :bytes)) Imgcodecs/CV_LOAD_IMAGE_COLOR))
        cards (solver/find-cards-intensity img (fn [& args]))
        _ (clojure.pprint/pprint cards)
        cards (map #(solver/identify-card img %) cards)
        cards (map #(assoc %1 :id %2) cards (range))
        resp {:size {:width (.width img) :height (.height img)}
              :cards (map #(select-keys % [:id :color :shape :count :fill :bb :debug]) cards)
              :sets (map (fn [s] (map #(select-keys % [:id]) s))
                         (solver/find-sets cards))}]
    (-> (r/response (json/write-str resp :value-fn mapify-bb))
        (r/content-type "application/json"))))

(defroutes app-routes
  ;(GET "/" [] "<h1>Hello</h1><form method=POST action=solve enctype=multipart/form-data><input type=file name=file><input type=submit></form>")
  (POST "/echo" {params :params} (echo params))
  (POST "/solve" {params :params} (solve params)))

(defn wrap-dir-index [handler]
  (fn [req]
    (handler
     (update-in req [:uri]
                #(if (= "/" %) "/index.html" %)))))

(def app
  (-> app-routes
      (wrap-resource "public")
      wrap-dir-index
      wrap-keyword-params
      (wrap-multipart-params {:store (byte-array-store)})
      wrap-params))


(comment

  (def server (hk/run-server #'app {:port 8080}))


  (server)

  )
