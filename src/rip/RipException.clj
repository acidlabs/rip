(ns rip.RipException
  (:gen-class
   :state state
   :init init
   :implements   [clojure.lang.IDeref]
   :constructors {[clojure.lang.APersistentMap] []}
   :extends java.lang.Exception))

(defn -init
  [error]
  [[] error])

(defn -deref
  [this]
  (.state this))
