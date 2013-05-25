(ns rip.core
  "Provides a resources abstraction."
  (:use compojure.core
        hiccup.util
        clout.core)
  (:require [clojure.string :as st]))

;;Routing

(defn scope*
  [name path]
  {:name       name
   :path       path
   :middleware []
   :type :scope})

(defmacro scope
  [name path & body]
  `(-> (scope* ~name ~path)
       ~@body))

(defmacro defscope
  [scope-name path & body]
  `(def ~scope-name
     (-> (scope ~(keyword (name scope-name)) ~path)
         ~@body)))

(defn route*
  [name path method handler]
  {:handler handler
   :name    name
   :method  method
   :path    path
   :type    :route})

(defmacro defroute
  [route-name path method & body]
  `(def ~route-name
     (route* ~(keyword (name route-name)) ~path ~method (h ~@body))))

(defmacro resources
  [res-name & body]
  `(-> (scope ~res-name ~(str "/" (name res-name)))
       ~@body))

(defmacro defresources
  [res & body]
  `(def ~res (resources ~(keyword (name res)) ~@body)))

(defn include*
  [scope path actions]
  (reduce
   (fn [scope [name action]]
     (assoc-in
      scope
      [:routes name]
      (assoc action
        :path
        (str path (:path action)))))
   scope
   actions))

(defmacro include
  [res path & body]
  `(include* ~res ~path (:routes (-> {:path ~path} ~@body))))

(defmacro member
  [res & body]
  `(include ~res "/:id" ~@body))

(defn nest
  [scope & routes]
  (reduce
   (fn [scope scope*]
     (assoc-in scope [:routes (:name scope*)]
               scope*))
   scope
   routes))

(defmacro nest-with
  [scope path & routes]
  `(include
    ~scope
    ~path
    (nest ~@routes)))

(defmacro nest-resources
  [scope item-name & routes]
  `(nest-with
    ~scope
    ~(str "/" item-name "-id")
    ~@routes))

;; Actions

(defmacro h
  "h is for handler, a macro for wrapping a function with bindings used by compojure's destructuring request forms"
  [bindings & body]
  `(fn [request#]
     (let-request [~bindings request#] ~@body)))

(defmacro defh
  "h is for handler, a macro for wrapping a function with bindings used by compojure's destructuring request forms"
  [name bindings & body]
  `(def ~name
     (fn [request#]
       (let-request [~bindings request#] ~@body))))

(defn action
  [scope name-path method handler]
  (let [[name path] (if (vector? name-path) name-path [name-path ""])]
    (assoc-in
     scope
     [:routes name]
     (route* name path method handler))))

(defmacro GET*
  [scope name & body]
  `(action ~scope ~name :get (h ~@body)))

(defmacro POST*
  [scope name & body]
  `(action ~scope ~name :post (h ~@body)))

(defmacro PUT*
  [scope name & body]
  `(action ~scope ~name :put (h ~@body)))

(defmacro DELETE*
  [scope name & body]
  `(action ~scope ~name :delete (h ~@body)))

(defmacro HEAD*
  [scope name & body]
  `(action ~scope ~name :head (h ~@body)))

(defmacro PATCH*
  [scope name & body]
  `(action ~scope ~name :patch (h ~@body)))

(defmacro OPTIONS*
  [scope name & body]
  `(action ~scope ~name :options (h ~@body)))

(defmacro ANY*
  [scope name & body]
  `(action ~scope ~name :any (h ~@body)))

(defmacro index
  [scope & body]
  `(GET* ~scope :index ~@body))

(defmacro make
  [scope & body]
  `(POST* ~scope :make ~@body))

(defmacro show
  [scope & body]
  `(GET* ~scope [:show "/:id"] ~@body))

(defmacro change
  [scope & body]
  `(PUT* ~scope [:change "/:id"] ~@body))

(defmacro destroy
  [scope & body]
  `(DELTE* ~scope [:destroy "/:id"] ~@body))

;; Other

(defn wrap*
  [scope name actions wrapper]
  (update-in scope [:middleware] conj [name actions wrapper]))

(defmacro wrap
  [scope name actions & body]
  `(wrap* ~scope
          ~name
          ~actions
          (fn [handler#]
            (-> handler#
                ~@body))))

(defn before-wrap*
  [scope before name actions wrapper]
  (assoc scope
    :middleware
    (reduce
     (fn [middlewares [before-name :as middleware]]
       (if (= before-name before)
         (conj middlewares [name actions wrapper] middleware)
         (conj middlewares middleware)))
     []
     (:middleware scope))))

(defmacro before-wrap
  [scope before name actions & body]
  `(before-wrap* ~scope
                 ~before
                 ~name
                 ~actions
                 (fn [handler#]
                   (-> handler#
                       ~@body))))

(defn after-wrap*
  [scope after name actions wrapper]
  (assoc scope
    :middleware
    (reduce
     (fn [middlewares [after-name :as middleware]]
       (if (= after-name after)
         (conj middlewares middleware [name actions wrapper])
         (conj middlewares middleware)))
     []
     (:middleware scope))))

(defmacro after-wrap
  [scope before name actions & body]
  `(after-wrap* ~scope
                 ~before
                 ~name
                 ~actions
                 (fn [handler#]
                   (-> handler#
                       ~@body))))

(defn add-middleware
  [scope]
  (reduce
   (fn [scope [_ actions middleware]]
     (reduce
      (fn [scope action]
        (update-in scope [:routes action :handler] middleware))
      scope
      (if (empty? actions)
        (keys (:routes scope))
        actions)))
   scope
   (reverse (:middleware scope))))

(defmulti route-for :type)

(defmethod route-for :route
  [{:keys [method path handler]}]
  (make-route method path handler))

(defmethod route-for :scope
  [scope]
  (apply
   routes
   (map
    (fn [route]
      (route-for
       (assoc route
         :path
         (str (:path scope) (:path route)))))
    (-> scope
        add-middleware
        :routes
        vals))))

(defmacro defapp
  [name & routes*]
  `(def ~name (apply routes (map route-for [~@routes*]))))

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

(defn compile-path
  [path params]
  (let [args (route-arguments path)
        path-params (take (count args) params)
        query-params (first (drop (count args) params))]
    (-> path
        (path-url (apply hash-map (interleave args params)))
        (query-url query-params))))

(defmulti link-for (fn [value & args] (:type value)))

(defmethod link-for :route
  [route & args]
  {:method (.toUpperCase (name (:method route)))
   :href   (compile-path (:path route) args)})

(defmethod link-for :scope
  [scope [route & routes] & args]
  (if-let [route (get-in scope [:routes route])]
    (let [route (assoc route :path (str (:path scope) (:path route)))]
      (if routes
        (apply link-for route (concat [routes] args))
        (apply link-for route args)))
    (throw (Exception. "Path not found"))))

(defn path-for
  [route & args]
  (:href (apply link-for (cons route args))))
