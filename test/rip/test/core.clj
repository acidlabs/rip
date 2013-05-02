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

(defentity users)
(defentity books
  (belongs-to users))

(def user-exists (h [id] (first (select users (where {:id id})))))

(defresources users*
  (assoc-name :users)

  ;;Middleware
  (wrap [] (wrap-macro dry-run))
  (wrap [:show :edit :destroy] (wrap-exists user-exists :user))
  (wrap [:create :edit] (wrap-body-parser :user))
  (wrap-keys :users :user)

  ;; GET /users | :index
  (index [] (select users))
  ;; POST /users | :create
  (create [user]
          (let [user (insert users (values user))]
            (create-response
             :user
             user
             (path-for users* [:show] (:id user)))))
  ;; GET /users/:id | :show
  (show [*user* id]
        (assoc-links
         *user*
         {:self [users* [:show] id]
          :edit [users* [:edit] id]}))
  ;; PUT /users/:id | :update
  (edit [id user] (update users (set-fields user) (where {:id id})))
  ;; DELETE /users/:id | :delete
  (destroy [id] (delete users (where {:id id})))

  ;; Nested resources
  (nest
   (resources
    :books
    (wrap [:index]
          (wrap-fn (fn [b] {:body {:books b}}))
          (wrap-exists user-exists :user)
          (wrap-macro dry-run))
    (index [users-id] (select books (where {:users-id users-id}))))))

(def users-route (route-for users*))

(users-route
 {:request-method :get
  :uri            "/users"})

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
  :uri            "/users/1/books"})
