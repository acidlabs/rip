(defproject rip "0.0.5"
  :description "REST in Peace, an unopinionated REST framework"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [compojure "1.1.0"]
                 [korma "0.3.0-beta9"]
                 [cheshire "4.0.0"]
                 [org.clojure/data.xml "0.0.3"]
                 [hiccup "1.0.0"]
                 [com.twinql.clojure/clj-conneg "1.1.0"]]
  :dev-dependencies [[codox "0.6.1"]]
  :aot [rip.RipException])
