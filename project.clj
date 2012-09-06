(defproject rip "0.0.7"
  :description "REST in Peace, an unopinionated REST framework"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [compojure "1.1.3"]
                 [korma "0.3.0-beta11"]
                 [cheshire "4.0.2"]
                 [org.clojure/data.xml "0.0.6"]
                 [hiccup "1.0.1"]
                 [com.twinql.clojure/clj-conneg "1.1.0"]]
  :plugins [[lein-swank "1.4.4"]
            [codox "0.6.1"]]
  :aot [rip.RipException])
