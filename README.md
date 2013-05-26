RIP
===
[![Build Status](https://travis-ci.org/acidlabs/rip.png?branch=master)](https://travis-ci.org/acidlabs/rip)

REST in Peace is a library for RESTful APIs built on top of compojure with some korma utilities.

## Installation

Add the following dependency to your `project.clj` file:

```clj
[rip "0.1.0"]
```

## Documentation

* [Wiki](https://github.com/acidlabs/rip/wiki)
* [API Docs](http://acidlabs.github.com/rip)

## Routes

```clojure
(ns app
  (:use rip.core
        compojure.core))

;; Single named route
(defroute home "/" :get [] "welcome")

;; Named nested routes
(defscope api
  "/api"

  ;; Routes can be defined using GET*, POST*, etc.
  ;; A name must be provided for wrappers and reverse routing
  ;; GET /api
  (GET* :home [] "api home")

  ;; Use a vector to specify a path
  ;; GET /api/about
  (GET* [:about "/about"] [] "a REST API for the win")

  ;; Include other actions with a default path
  (include
   "/paths"

   ;; Obtain paths for actions
   (GET* :paths []
         (str (path-for api [:home])
              " and "
              (path-for api [:about]))
         ;;=> /api and /api/about
         ))

  ;; Nest other scopes or resources
  (nest

   ;; Resources are scopes with path "/{resource-name}", check also 'defresource'
   (resources
    :users
    ;; Rip includes some default actions like:
    ;;   index   => GET    /
    ;;   make    => POST   /
    ;;   show    => GET    /:id
    ;;   change  => PUT    /:id
    ;;   destroy => DELETE /:id
    ;; Examples:
    (index [] "users")
    (change [id] (str "user " id " changed"))

    ;; Obtain links
    (show [id]
          (str {:links
                {:change (link-for api [:users :change] id)}}))
    ;; => {:links {:change {:method "PUT" :href "/api/users/{user id}"}}}

    ;; Include other actions with default /:id path
    ;; Same as using (include "/:id")
    (member
     ;; PATCH /api/users/:id/activate
     (PATCH* [:activate "/activate"] [id] (str "user " id " activated")))

    ;; Nest resources passing the member key
    (nest-resources
     :user
     (resources
      :documents

      ;; GET /api/users/:user-id/documents
      (index [user-id] (str "documents for user " user-id)))))))

;; Generate handler
(defroutes app
  (route-for home)
  (route-for api))
```
