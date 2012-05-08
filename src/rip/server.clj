(ns rip.server
  (:use [rip.middleware])
  (:require [noir.server :as server]))

(server/load-views "src/rip/interface/")

(defn -main [& m]
  (let [mode (keyword (or (first m) :dev))
        port (Integer. (get (System/getenv) "PORT" "8080"))]
    (server/add-middleware wrap-input-param)
    (server/add-middleware wrap-authentication)
    (server/start port {:mode mode
                        :ns 'rip})))
