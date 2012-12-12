(ns rip.test.validation
  (:use rip.validation
        korma.core
        clojure.test))

(declare books users)

(defentity users
  (belongs-to [:parent users])
  (has-many books))

(defentity books
  (belongs-to users))

(def filter-users
  (query-validator users
                   {:name    String
                    :age     Integer
                    :address {:city [:address_city String]
                              :street [:address_street String]}
                    :books   (query-validator books
                                              {:name String
                                               :year Long})}))

(deftest validate-query
  (is  (= (try (sql-only
                (exec (filter-users
                       {:name ["sebastian"]
                        :age  [{:lt (int 30)}]
                        :books {:name [{:lk "game of thrones"}]}})))
               (catch Exception e (.printStackTrace  e)))
          "SELECT \"users\".* FROM \"users\" WHERE ((\"users\".\"name\" = ?) OR (\"users\".\"age\" < ?))")))

(run-tests)
