(defproject rip "0.0.9"
  :description "REST in Peace, a framework for RESTful API development."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.3"]
                 [cheshire "4.0.2"]
                 [org.clojure/data.xml "0.0.6"]
                 [hiccup "1.0.1"]
                 [clout "1.1.0"]
                 [ring/ring-core "1.1.5"]
                 [com.twinql.clojure/clj-conneg "1.1.0"]]
  :profiles {:dev {:dependencies
                   [[korma "0.3.0-RC5"]
                    [http-kit "2.0.1"  :exclusions [org.clojure/clojure]]
                    [ring-mock "0.1.3" :exclusions [org.clojure/clojure]]]}}
  :plugins [[codox "0.6.1"]]
  :aot [rip.RipException])
