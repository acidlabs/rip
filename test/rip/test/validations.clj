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
  ;; (is  (= (try (sql-only
  ;;               (exec (filter-users
  ;;                      {:name ["sebastian"]
  ;;                       :age  [{:lt (int 30)}]
  ;;                       :books {:name [{:lk "game of thrones"}]}})))
  ;;              (catch Exception e (.printStackTrace  e)))
  ;;         "SELECT \"users\".* FROM \"users\" WHERE ((\"users\".\"name\" = ?) OR (\"users\".\"age\" < ?))"))
  (is (= ((validator
           {:name   (type-of String)
            :phones (list-of String)
            :books  (list-of (validator {:year (type-of String)}))}
           {:required    [:name]
            :constraints {:name [(max-size 30) (min-size 10)]}})
          {:name "sd" :phones ["555-555-555" 3] :books [{:year 2012}]})
         {:valid? false
          :input  {:name {:input  "sd"
                          :errors
                          '({:message "The length must be at least 10" :code :min-size})}
                   :phones [{:input  3
                             :valid? false
                             :errors [{:message "Invalid type" :code :type-error}]}]
                   :books [{:input
                            {:year
                             {:errors [{:message "Invalid type" :code :type-error}]
                              :input 2012}}}]}
          :output {:phones ["555-555-555"] :books []}})))

(run-tests)
