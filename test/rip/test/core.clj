(ns rip.test.core
  (:use rip.core
        rip.util
        rip.middleware
        ring.util.response
        clojure.test
        cheshire.core
        ring.middleware.params
        compojure.core
        [korma.core :exclude (nest create)]))

(declare users*)
(defentity users)
(defentity books
  (belongs-to users))

(defh users-count []
  (-> (select users (aggregate (count :*) :total))
      first
      :total
      (or 100)))

(defh user-exists [id] (first (select users (where {:id id}))))

(defn assoc-user-links
  [{id :id :as user}]
  (assoc-links
   user
   {:self [users* [:show] id] :edit [users* [:edit] id]}))

(defroute action
  :get
  (h [] "hola"))

(defscope users-scope
  (path "/users")
  (get! :index [] "asdasd")
  (post! :create [] "")
  (include
   "/:id"
   (get! :show [])
   (put! :edit [])
   (delete! :destroy []))
  (wrap [:show :edit :destroy] (wrap-exists? user-exists :user))
  (wrap [:create :edit] (wrap-body-parser :user))
  (wrap [] (wrap-fn (fn [b] {:body b})))
  (wrap [:create :show :edit] (wrap-fn (fn [u] {:user (assoc-user-links u)})))
  (wrap-collection :index [users-scope [:index]] users-count))

(defresources users*
  (assoc-name :users)
  ;;Middleware
  (wrap [:show :edit :destroy] (wrap-exists? user-exists :user))
  (wrap [:create :edit] (wrap-body-parser :user))
  (wrap [] (wrap-fn (fn [b] {:body b})))
  (wrap [:create :show :edit] (wrap-fn (fn [u] {:user (assoc-user-links u)})))
  (wrap-collection :index [users* [:index]] users-count)
  ;; GET /users | :index
  (index [*limit* *offset* *links*]
         {:users (-> {:list (select users (limit *limit*) (offset *offset*))}
                     (assoc-links *links*))})
  ;; POST /users | :create
  (create [user] (first (insert users (values user))))
  ;; GET /users/:id | :show
  (show [*user*] *user*)
  ;; PUT /users/:id | :update
  (edit [user id] (first (update users (set-fields user) (where {:id id}))))
  ;; DELETE /users/:id | :delete
  (destroy [id] (delete users (where {:id id})) "User deleted")
  ;; Nested resources
  (nest
   (resources
    :books
    ;; Custom actions
    (action :example :get :member
            (h [users-id id] (str "book " id " user " users-id))))))

(def users-route (-> (route-for users*) (wrap-macro dry-run) wrap-params))

(users-route
 {:request-method :get
  :uri            "/users"
  :query-string   "page=1&per_page=10"})

(users-route
 {:request-method :post
  :uri            "/users"
  :body           (.getBytes (generate-string {:user {:name "sebastian"}}))
  :headers        {"content-type" "application/json"}})

(users-route
 {:request-method :put
  :uri            "/users/1"
  :body           (.getBytes (generate-string {:user {:name "karla"}}))
  :headers        {"content-type" "application/json"}})

(users-route
 {:request-method :get
  :uri            "/users/1"})

(users-route
 {:request-method :delete
  :uri            "/users/1"})

(users-route
 {:request-method :get
  :uri            "/users/1/books/2/example"})
