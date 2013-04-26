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

(defresources users
  (wrap []
        ((fn [h] (fn [r] (str (h r))))))
  (wrap [:index]
        ((fn [h] (fn [r] {:users (h r)}))))
  (action :member :get :go
          (fn [r] (get-in r [:params :id])))
  (index [] [1 2 3])
  (nest
   (resources :books
              (action :go :get :member
                      (h [id users-id :as {:keys [*user]}]
                         (str
                          "sfsdf"
                          id
                          users-id))))))

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
;;   (wrap [:create]
;;         (response resp (assoc resp :status 201)))
;;   (wrap [:index]
;;         (response users {:users resp}))
;;   (wrap [:show]
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
;;   (wrap [:show]
;;         (response
;;          user
;;          (if (:active user)
;;            user
;;            ;; Use the links function tu add HATEOAS links
;;            (hateoas
;;             user
;;             {:activate [users [:activate] (:id user)]
;;              :books    [users [:books :index] (:id user)]}))))

;;   ;; Titles for HATOAS links
;;   (titles
;;    {:activate "Activates user"}))

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

(defn hateoas
  []
  )
