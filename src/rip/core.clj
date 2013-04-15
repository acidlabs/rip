(ns rip.core
  "Provides a resources abstraction."
  (:use compojure.core
        hiccup.util
        clojure.tools.macro
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
(defn action
  [res type method action handler]
  (let [[action path] (if (vector? action)
                        action
                        [action (str "/" (name action))])
        path (str (get-in res [:paths type]) path)]
    (assoc-in res
              [:actions action]
              {:method  method
               :path    path
               :handler (make-route method path handler)})))

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

(defmacro routefn
  "A macro for wrapping a function definition with bindings used by compojure's destructuring request forms"
  [bindings & body]
  `(fn [request#]
     (let-request [~bindings request#] ~@body)))

(defmacro index
  [res bindings & body]
  `(action
    ~res
    :collection
    :get
    [:index ""]
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro create
  [res bindings & body]
  `(action
    ~res
    :collection
    :post
    [:create ""]
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro show
  [res bindings & body]
  `(action
    ~res
    :member
    :get
    [:show ""]
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro update
  [res bindings & body]
  `(action
    ~res
    :member
    :put
    [:update ""]
    (fn [request#]
      (let-request [~bindings request#] ~@body))))

(defmacro delete
  [res bindings & body]
  `(action
    ~res
    :member
    :delete
    [:delete ""]
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
  [res actions & middlewares]
  (reduce
   (fn [res action]
     (update-in
      res
      [:actions action]
      (fn [f]
        (reduce (fn [handler wrapper]
                  (wrapper handler))
                f
                middlewares))))
   res
   actions))

(defmacro wrap
  [])

(defresources users
  (action :member :get :go
          (fn [r] (get-in r [:params :id])))
  (actions :collection
           [:get [:index ""] (fn [_] "ascac")])
  (nest
   (resources :books
              (action :member :get :go
                      (fn [r]
                        (str
                         "sfsdf"
                         (get-in r [:params :id])
                         (get-in r [:params :users-id])))))))

(defn route-for
  [res]
  (apply
   routes
   (concat
    (map :handler (vals (:actions res)))
    (if-let [nested (not-empty (:nested res))]
      [(context (get-in res [:paths :nested]) []
                (apply routes (map route-for (vals nested))))]
      []))))

((route-for users)
 ;(get-in users [:actions :index :handler])
 {:request-method :get :uri "/users/1/books/2/go"})

;; (defresources users

;;   ;; Default CRUD actions. Use compojure's request destructuring syntax
;;   ;; GET /users | :index
;;   (index [] (k/select users-model))
;;   ;; POST /users | :create
;;   (create [user] (k/insert users-model user))
;;   ;; GET /users/:id | :show
;;   (show [id] (first (k/select users-model (where {:id id}))))
;;   ;; PUT /users/:id | :update
;;   (update [id user] (first (k/update users-model )))
;;   ;; DELETE /users/:id | :delete
;;   (delete [id] )

;;   ;; Add other actions to /users
;;   (collection
;;    ;; GET /users/report | :report
;;    [:get :report (handler [] "TODO")])

;;   ;; Add other actions to /users/:id
;;   (member
;;    ;; Specify action name and path
;;    ;; PUT /users/:id/action | :activate
;;    [:put [:activate "/action"] (handler [id] "TODO")])

;;   ;; Nest other resources in /users/:id
;;   (nest-resources
;;    (resources :books
;;               ;; GET /users/:users-id/books
;;               (index [users-id] (k/select books-model (where {:users-id users-id})))
;;               ;; GET /users/:id/books/:books-id
;;               (show [id] (k/select books-model (where {:id id})))))

;;   ;; Wrapping middlewares
;;   ;; (middlewares will be executed in the same order they are added)

;;   ;; Add middlewares to all actions
;;   (wrap []
;;         (rip/content-types [:json :xml] :json)
;;         (response resp {:body resp}))

;;   ;; Add middlewares to a sigle action
;;   (wrap :create
;;         (response resp (assoc resp :status 201)))
;;   (wrap :index
;;         (response users {:users resp}))
;;   (wrap :show
;;         (response user {:user resp}))

;;   ;; Add middlewares to a group of actions
;;   (wrap [:show :update]
;;         (conditional
;;          (handler [id] (nil? (find users id)))
;;          :not-found))

;;   ;; Validations
;;   ;; Using a schema
;;   (wrap [:create :update]
;;         (validate-schema :user {:name String}))
;;   ;; Custom validations
;;   (wrap :create
;;         (validate :user
;;                   (fn [user] (nil? (:name user)))
;;                   "Name required"))

;;   ;; HATEOAS links
;;   (wrap :show
;;         (response
;;          user
;;          (if (:active user)
;;            user
;;            ;; Use the links function tu add HATEOAS links
;;            (hateoas
;;             user
;;             {:activate [[users :activate] (:id user)]
;;              :books    [[users :books :show] (:id user)]}))))

;;   ;; Titles for HATOAS links
;;   (titles
;;    {:activate "Activates user"}))

;; (defn resources
;;   "Similar to rails resources, provides a way to define routes based on RESTful like actions.
;;    Receives a name and a series of actions defined through functions memb and/or coll.
;;    Usage:
;;           (resources users
;;                      (wrap
;;                        wrap-collection)
;;                      (action :index
;;                              :get
;;                              (fn [] )
;;                              (wrap ()))
;;                      (action :create
;;                              :post)
;;                      (items
;;                         (wrap (find ))
;;                         (action show
;;                                 :get
;;                                 )
;;                         )
;;                      (resources ))"
;;   [name res-opt & opts]
;;   (gen-resources (Resources. name nil nil res)))

;; (defn memb
;;   "Creates a route of a member to be passed to a resources definition.
;;    Usage:
;;           (resources :users (memb :show :get identity))
;;            => /users/:id
;;    Options:
;;           - sufix
;;           - url-handler"
;;   [res-name method route-handler & [{:keys [sufix url-handler]}]]
;;   (Member. method res-name "" sufix route-handler (if url-handler url-handler identity)))

;; (defn coll
;;   "Creates a route to the collection of a resources definition.
;;    Usage:
;;           (resources :users (memb :show :get identity))
;;            => /users/:id
;;    Options:
;;           - sufix
;;           - url-handler"
;;   [res-name method route-handler & [{:keys [sufix url-handler]}]]
;;   (Collection. method res-name "" sufix route-handler (if url-handler url-handler identity)))

;; (defn route
;;   "Creates a route object"
;;   [method path route-handler & [url-handler]]
;;   (Route. method path route-handler (if url-handler url-handler identity)))

;; (defmulti ->handler
;;   "Generates the ring handler function for the passed resource"
;;   type)

;; (defmethod ->handler rip.core.Resources
;;   [{:keys [resources]}]
;;   (apply routes (map ->handler (vals resources))))

;; (defmethod ->handler rip.core.Collection
;;   [{:keys [method path route-handler]}]
;;   (make-route method (clout.core/route-compile path) route-handler))

;; (defmethod ->handler rip.core.Member
;;   [{:keys [method path route-handler]}]
;;   (make-route method (clout.core/route-compile path) route-handler))

;; (defmethod ->handler rip.core.Route
;;   [{:keys [method path route-handler]}]
;;   (make-route method (clout.core/route-compile path) route-handler))

;; (defn- throwf [msg & args]
;;   (throw (Exception. (apply format msg args))))

;; (defn- ^{:skip-wiki true} route-arguments
;;   "returns the list of route arguments in a route"
;;   [route]
;;   (let [args (re-seq #"/(:([^\/\.]+)|\*)" route)]
;;     (set (map #(keyword (or (nth % 2) (second %))) args))))

;; (defn- path-url [url route-args]
;;   (let [url (if (vector? url) ;;handle complex routes
;;               (first url)
;;               url)
;;         route-arg-names (route-arguments url)]
;;     (when-not (every? (set (keys route-args)) route-arg-names)
;;       (throwf "Missing route-args %s" (vec (filter #(not (contains? route-args %)) route-arg-names))))
;;     (reduce (fn [path [k v]]
;;               (if (= k :*)
;;                 (string/replace path "*" (str v))
;;                 (string/replace path (str k) (str v))))
;;             url
;;             route-args)))

;; (defn- query-url [uri params]
;;   (str (url uri params)))

;; (defn ->url
;;   "Returns the url belonging to the passed resource.
;;    Usage:
;;           (let [users (resources :users
;;                                  (resources :documents (coll :index :get (fn [req] \"User documents\"))))]
;;             (-> url [users :documents :index] {:route-params {:id 1}}))"
;;   [resource & [{:keys [route-params query-params]}]]
;;   (let [{:keys [path url-handler]} (if (vector? resource)
;;                                      (reduce (fn [x1 x2] (x2 (:resources x1)))
;;                                              resource)
;;                                      resource)]
;;     (url-handler
;;      (query-url
;;       (path-url path
;;                 route-params) query-params))))

;; (defmacro defresources
;;   "Creates a named resources value in the namespace and returns the handler"
;;   [res-name & args]
;;   `(do (def ~res-name (resources ~(keyword (str res-name)) ~@args))
;;        (->handler ~res-name)))


;; (defrecord Route [method path route-handler url-handler])

;; (defrecord Member [method res-name path sufix route-handler url-handler])

;; (defrecord Collection [method res-name path sufix route-handler url-handler])

;; (defrecord Resources [res-name path id resources])

;; (defn- gen-resources
;;   [{:keys [res-name resources] :as res} & [parent-path]]
;;   (let [path        (if parent-path (str parent-path "/" (name res-name)) (str "/" (name res-name)))
;;         id          (if parent-path (str ":" (name res-name) "-id") ":id")
;;         member-path (str path "/" id)]
;;     (assoc res
;;       :path path
;;       :id id
;;       :resources
;;       (reduce
;;        (fn [resources resource]
;;          (assoc resources (:res-name resource)
;;                 (condp #(= (type %2) %1) resource
;;                   Member     (assoc resource :path
;;                                     (str member-path (if-let [sufix (:sufix resource)]
;;                                                        (str "/" (name sufix)))))
;;                   Collection (assoc resource :path
;;                                     (str path (if-let [sufix (:sufix resource)]
;;                                                 (str "/" (name sufix)))))
;;                   Resources  (gen-resources resource member-path)
;;                   (throw (Exception. "A resource must be either of type Member, Collection or Resources")))))
;;        {}
;;        (if (map? resources) (vals resources) resources)))))
