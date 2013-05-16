(ns rip.core
  "Provides a resources abstraction."
  (:use compojure.core
        hiccup.util
        clout.core)
  (:require [clojure.string :as st]))

;;Resources

(defn scope
  [name path & children]
  {:type :scope
   :path path
   :name name
   :children (reduce {} children)})

(defmacro defscope
  [name path & children]
  `(def ~name (scope (keyword ~name) path ~@children)))

(defn resources*
  [res-name]
  {:name res-name
   :path (str "/" (name res-name))
   :nested {}
   :middleware []
   :type :resources})

(defn add-fn
  [res f]
  (update-in res [:fns] conj f))

(defmacro resources
  [name & body]
  `(-> (resources* ~name)
       ~@body))

(defmacro defresources
  [res & body]
  `(do
     (declare ~res)
     (def ~res
       (resources ~(keyword (name res)) ~@body))))

(defn route
  [name method path handler]
  {:handler (make-route method path handler)
   :method method
   :path path
   :name name
   :type :route})

(defmacro defroute
  [name & args]
  `(def ~name (route (keyword ~name) ~@args)))

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
  [res name-path method handler]
  (let [[name path] (if (vector? name-path) name-path [name-path ""])
        path (str (:path res) path)]
    (assoc-in res
              [:actions name]
              {:method  method
               :path    path
               :handler handler})))

(defn include*
  [res path actions]
  (reduce
   (fn [res [name action]]
     (assoc-in res
               [:actions name]
               (assoc action
                 :path
                 (str (:path res) path (:path action)))))
   res
   actions))

(defmacro include
  [res path & body]
  `(include* res path (:actions (-> {:path path} ~@body))))

(defmacro member
  [res & body]
  `(include res "/:id" ~@body))

(defn nest
  [res path & res*]
  (reduce
   (fn [res res*]
     (assoc-in res [:nested (:name res*)]
               [path res*]))
   res
   res*))

(defmacro get!  [res name & body] `(action ~res ~name :get (h ~@body)))

(defmacro post! [res name & body] `(action ~res ~name :post (h ~@body)))

(defmacro put! [res name & body] `(action ~res ~name :put (h ~@body)))

(defmacro delete! [res name & body] `(action ~res ~name :delete (h ~@body)))

(defmacro head! [res name & body] `(action ~res ~name :head (h ~@body)))

(defmacro patch! [res name & body] `(action ~res ~name :patch (h ~@body)))

(defmacro options! [res name & body] `(action ~res ~name :options (h ~@body)))

(defmacro any! [res name & body] `(action ~res ~name :any (h ~@body)))

(defmacro index [res & body] `(get! ~res :index ~@body))

(defmacro make [res & body] `(post! ~res :make ~@body))

(defmacro show [res & body] `(get! ~res [:show "/:id"] ~@body))

(defmacro change [res & body] `(put! ~res [:change "/:id"] ~@body))

(defmacro destroy [res & body] `(delete! ~res [:destroy "/:id"] ~@body))

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

(defmulti route-for :type)

(defmethod route-for :route [route] (:handler route))

(defmethod route-for :scope
  [scope]
  (context (:path scope) []
           (map route-for (:routes scope))))

(defmethod route-for :resources
  [res]
  (let [handler (apply routes (-> res add-middleware make-handlers))]
    (if-let [nested (not-empty (:nested res))]
      (;;apply
       routes
       handler
       ;; (map
       ;;  (fn [[path resources]]
       ;;    (context path [] (route-for resources)))
       ;;  nested)
       )
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

(defmulti path-for (fn [value & args] (:type value)))

;; (defmethod path-for :scope
;;   [scope & [child & args]]
;;   (if-let []
;;     ))

(defmethod path-for :resources
  [res & [args & params]]
  (let [path (make-path res args)
        args (route-arguments path)
        path-params (take (count args) params)
        query-params (first (drop (count args) params))]
    (-> path
        (path-url (apply hash-map (interleave args params)))
        (query-url query-params))))

(defresources poto
  (get! :index [] (str poto))
  (make [] "sfdsf"))

(defresources asdf
  (get! :index [] (path-for asdf [:index]))
  (make [] "sfdsf"))

((route-for asdf) {:request-method :get :uri "/asdf"})
