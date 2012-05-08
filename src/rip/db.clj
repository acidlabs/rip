(ns rip.validation)

(defn min-length [min msg] (fn [v] [v (when (< (count v) min) msg)]))
(defn max-length [max msg] (fn [v] [v (when (> (count v) max) msg)]))
(defn range-length [min max msg] (fn [v] [v (when (or (< (count v) min) (> (count v) max)) msg)]))

(defn min [min msg] (fn [v] [v (when (< v min) msg)]))
(defn max [max msg] (fn [v] [v (when (> v max) msg)]))
(defn range [min max msg] (fn [v] [v (when (or (< v min) (> v max)) msg)]))

(defn not-null [msg] (fn [v] [v (when (nil? v) msg)]))

(defn entity-list? []
  (fn []))

()
