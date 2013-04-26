(ns rip.test.core
  (:use rip.core
        rip.util
        rip.middleware
        clojure.test)
  (:require [korma.core :as k]))

(defresources users
  (wrap []
        ((fn [h] (fn [r] (str (h r)))))
        ((fn [h] (fn [r] (h (assoc-globals r {:a 1}))))))
  (wrap [:index]
        ((fn [h] (fn [r] {:users (h r)}))))
  (action :member :get :go
          (fn [r] (get-in r [:params :id])))
  (index [_a] _a)
  (nest
   (resources :books
              (action :go :get :member
                      (h [id users-id :as {:keys [*user]}]
                         (str
                          "sfsdf"
                          id
                          users-id))))))

((route-for users)
 ;;(get-in users [:actions :index :handler])
 {:request-method :get :uri "/users"})

(k/defentity users-model)

(k/defentity books-model)

(defresources users

  ;; Default CRUD actions. Use compojure's request destructuring syntax
  ;; GET /users | :index
  (index [] (k/select users-model))
  ;; POST /users | :create
  (create [user] (k/insert users-model user))
  ;; GET /users/:id | :show
  (show [id] (first (k/select users-model (k/where {:id id}))))
  ;; PUT /users/:id | :update
  (update [id user] (first (k/update users-model )))
  ;; DELETE /users/:id | :delete
  (delete [id] )

  ;; Add other actions to /users
  (collection
   ;; GET /users/report | :report
   [:get :report (h [] "TODO")])

  ;; Add other actions to /users/:id
  (member
   ;; Specify action name and path
   ;; PUT /users/:id/action | :activate
   [:put [:activate "/action"] (h [id] "TODO")])

  ;; Nest other resources in /users/:id
  (nest
   (resources :books
              ;; GET /users/:users-id/books
              (index [users-id] (k/select books-model (k/where {:users-id users-id})))
              ;; GET /users/:id/books/:books-id
              (show [id] (k/select books-model (k/where {:id id})))))

  ;; Wrapping middlewares
  ;; (middlewares will be executed in the same order they are added)

  ;; Add middlewares to all actions
  (wrap []
        ;;(content-types [:json :xml] :json)
        (wrap-response resp {:body resp}))

  ;; Add middlewares to a sigle action
  (wrap [:create]
        (wrap-response resp (assoc resp :status 201)))
  (wrap [:index]
        (wrap-response resp {:users resp}))
  (wrap [:show]
        (wrap-response resp {:user resp}))

  ;; Add middlewares to a group of actions
  (wrap [:show :update]
        (wrap-conditional
         (h [id] (nil? (find users id)))
         :not-found))

  ;; Validations
  ;; Using a schema
  ;; (wrap [:create :update]
  ;;       (validate-schema :user {:name String}))
  ;; ;; Custom validations
  ;; (wrap :create
  ;;       (validate :user
  ;;                 (fn [user] (nil? (:name user)))
  ;;                 "Name required"))

  ;; HATEOAS links
  (wrap [:show]
        (wrap-response
         user
         (if (:active user)
           user
           ;; Use the links function tu add HATEOAS links
           (hateoas
            user
            {:activate [users [:activate] (:id user)]
             :books    [users [:books :index] (:id user)]})))))
