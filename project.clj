(defproject rip "1.0.0-SNAPSHOT"
  :description "REST in Peace, an unopinionated REST framework"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [noir "1.2.2"]
                 [cheshire "4.0.0"]
                 [org.clojure/data.xml "0.0.3"]
                 [clojureql "1.0.3"]
                 [ring-basic-authentication "0.0.1"]]
  :main rip.server)
