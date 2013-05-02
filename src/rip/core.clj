(ns rip.core
  "Provides a resources abstraction."
  (:use compojure.core
        hiccup.util
        clout.core)
  (:require [clojure.string :as st]))

;; Resources
(defn assoc-name
  [res res-name]
  (let [path (str "/" (name res-name))]
    {:name   res
     :paths  {:collection path
              :member     (str path "/:id")
              :nested     (str path "/:" (name res-name) "-id")}}))

(defn resources*
  [res]
  (assoc-name
    {:route  nil
     :nested {}
     :titles {}
     :middleware []}
    res))

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

(defmacro edit
  [res bindings & body]
  `(action
    ~res
    [:edit ""]
    :put
    :member
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro destroy
  [res bindings & body]
  `(action
    ~res
    [:destroy ""]
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
  (update-in res [:middleware] conj [actions middleware]))

(defmacro wrap
  [res actions & body]
  `(wrap* ~res
          ~actions
          (fn [handler#]
            (-> handler#
                ~@body))))

(defn add-middleware
  [res]
  (reduce
   (fn [res [actions middleware]]
     (reduce
      (fn [res action]
        (update-in res [:actions action :handler] middleware))
      res
      (if (empty? actions)
        (keys (:actions res))
        actions)))
   res
   (:middleware res)))

(defn make-handlers
  [res]
  (map
   (fn [{:keys [method path handler]}]
     (make-route method path handler))
   (vals (:actions res))))

(defn route-for
  [res]
  (let [handler (apply routes (-> res add-middleware make-handlers))]
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
