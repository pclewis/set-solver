(ns set-solver.reusable-buffer
  (:require [clojure.core.cache :as cache])
  (:import [java.util.UUID]))

(def cache (atom (cache/ttl-cache-factory {} :ttl 2000)))

(defn update-cache [c k v]
  (if (cache/has? c k)
    (cache/miss c k (get c k)) ; use miss anyway to update TTL
    (cache/miss c k (v))))

(defmacro reusable [constructor]
  (let [id (java.util.UUID/randomUUID)]
    `(get (swap! cache update-cache ~id (fn [] ~constructor)) ~id)))
