(ns rip.core
  "Provides a resources abstraction."
  (:use compojure.core
        hiccup.util
        clout.core)
  (:require [clojure.string :as st]))

;; Resources
(defn resources*
  [res]
  (let [path (str "/" (name res))]
    {:name   res
     :route  nil
     :paths  {:collection path
              :member     (str path "/:id")
              :nested     (str path "/:" (name res) "-id")}
     :nested {}
     :titles {}}))

(defmacro resources
  [res & body]
  `(-> (resources* ~res)
       ~@body))

(defmacro defresources
  ""
  [res & body]
  `(def ~res
     (-> (resources ~(keyword (name res)))
         ~@body)))

;; Actions

(defmacro h
  "h is for handler, a macro for wrapping a function definition with bindings used by compojure's destructuring request forms"
  [bindings & body]
  `(fn [request#]
     (let-request [~bindings request#] ~@body)))

(defn action
  [res action method type handler]
  (let [[action path] (if (vector? action)
                        action
                        [action (str "/" (name action))])
        path (str (get-in res [:paths type]) path)]
    (assoc-in res
              [:actions action]
              {:method  method
               :path    path
               :handler handler})))

(defn actions
  [res type & actions]
  (reduce
   (fn [res params]
     (apply action (concat [res type] params)))
   res
   actions))

(defn collection
  [res & acts]
  (apply actions res acts))

(defn member
  [res & acts]
  (apply actions res acts))

(defmacro index
  [res bindings & body]
  `(action
    ~res
    [:index ""]
    :get
    :collection
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro create
  [res bindings & body]
  `(action
    ~res
    [:create ""]
    :post
    :collection
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro show
  [res bindings & body]
  `(action
    ~res
    [:show ""]
    :get
    :member
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro update
  [res bindings & body]
  `(action
    ~res
    [:update ""]
    :put
    :member
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro delete
  [res bindings & body]
  `(action
    ~res
    [:delete ""]
    :delete
    :member
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defn nest
  [res & res*]
  (reduce
   (fn [res res*]
     (assoc-in res [:nested (:name res*)]
               res*))
   res
   res*))

;; Other

(defn wrap*
  [res actions middleware]
  (if (empty? actions)
    (update-in
     res
     [:middleware]
     (fn [m] (if m (middleware m) middleware)))
    (reduce
     (fn [res action]
       (update-in
        res
        [:actions-middleware action]
        (fn [mw]
          (if mw
            (fn [h] (-> h mw middleware))
            middleware))))
     res
     actions)))

(defmacro wrap
  [res actions & body]
  `(wrap* ~res
          ~actions
          (fn [handler#]
            (-> handler#
                ~@body))))

(defn make-handlers
  [res]
  (let [res-middleware(:middleware res)]
    (map
     (fn [[name {:keys [method handler path]}]]
       (let [middleware (get-in res [:actions-middleware name])
             handler (if middleware (middleware handler) handler)]
         (make-route
          method
          path
          (if res-middleware (res-middleware handler) handler))))
     (:actions res))))

(defn route-for
  [res]
  (let [handler (apply routes (make-handlers res))]
    (if-let [nested (not-empty (:nested res))]
      (routes
       handler
       (context (get-in res [:paths :nested]) []
                (apply routes (map route-for (vals nested)))))
      handler)))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- ^{:skip-wiki true} route-arguments
  "returns the list of route arguments in a route"
  [route]
  (let [args (re-seq #"/(:([^\/\.]+)|\*)" route)]
    (map #(keyword (or (nth % 2) (second %))) args)))

(defn- path-url [url route-args]
  (let [url (if (vector? url) ;;handle complex routes
              (first url)
              url)
        route-arg-names (route-arguments url)]
    (when-not (every? (set (keys route-args)) route-arg-names)
      (throwf "Missing route-args %s" (vec (filter #(not (contains? route-args %)) route-arg-names))))
    (reduce (fn [path [k v]]
              (if (= k :*)
                (st/replace path "*" (str v))
                (st/replace path (str k) (str v))))
            url
            route-args)))

(defn- query-url [uri params]
  (str (url uri params)))

(defn- make-path
  [res [action & actions] & [path]]
  (if-let [x (get-in res [:actions action :path])]
    (if actions
      (throw (Exception. "Path not found"))
      (str path x))
    (let [x (get-in res [:nested action])]
      (if (and x actions)
        (make-path
         x
         actions
         (str path (get-in res [:paths :nested])))
        (throw (Exception. "Path not found"))))))

(defn path-for
  [res args & params]
  (let [path (make-path res args)
        args (route-arguments path)
        path-params (take (count args) params)
        query-params (first (drop (count args) params))]
    (-> path
        (path-url (apply hash-map (interleave args params)))
        (query-url query-params))))
