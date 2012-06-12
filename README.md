rip
===

REST in Peace is a library for RESTful APIs built on top of compojure with some korma utilities.

[API] (http://acidlabs.github.com/rip)

## Installation

Add depencency to lein project:

```clj
[rip "0.0.5"]
```

## Concepts

### Actions
rip provides a ```defaction ``` macro for high level REST actions definitions like **show**, **add**, etc.
It generates a compojure method handler (GET, POST, etc.), and a reverse routing function for HATEOAS like links.

Example:
```clj
(defaction show-user "/users/:id" 
  [:get
   (fn [{{id :id} :params}] (find-by-id users id))
   (fn [url] {:show {:href url}})])
```
This generates a compojure GET handler **show-user** and a reverse routing function **show-user->url**.

Some predefined actions are available in ```rip.action```

### Validations

A body validation function is provided to validate clojure maps from json or xml content.

Example:
```clj
(body-validator
  {:name    (required String (max-length 30))
   :phones  (required [String] (min-length 1))
   :books   (optional [(body-validator {:year (required string->date)})])
   :address (optional (body-validator
                        {:city (required String)
                         :street (optional String)}))})
```
Also a function to generate korma queries from clojure maps.

Example:
```clj
(query-validator
  {:name    identity
   :address {:city [:address_city city-validator]
             :street :address_street}
   :books   (query-validator
              {:name identity
               :year date-validatior})})
```
