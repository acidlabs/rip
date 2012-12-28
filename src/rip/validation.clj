(ns rip.validation
  "Validation functions for body input and filters for collection queries"
  (:import rip.RipException)
  (:use korma.sql.fns
        korma.core
        rip.db)
  (:require [clojure.string :as st]))

;; Body validation

(def ^{:dynamic true} *default-messages*
  {:min-size       "The length must be at least %s"
   :max-size       "The length must be up to %s"
   :range-size     "The length must be between %s and %s"
   :type-error     "Invalid type"
   :required-error "This field is required"
   :max            "It must be up to %s"
   :min            "It must be at least %s"
   :range          "It must be between %s and %s"
   :no-error       ""})

(defn gen-validation
  [test code & msg-params]
  {:valid? test :errors [{:message (apply format (code *default-messages*) msg-params) :code code}]})

;; Collections and strings constraints

(defn min-size [min] (fn [val] (gen-validation (>= (count val) min) :min-size min)))
(defn max-size [max] (fn [val] (gen-validation (<= (count val) max) :max-size max)))
(defn range-size [min max] (fn [val] (gen-validation (or (>= (count val) min)
                                                        (<= (count val) :range-size min max)))))

;; Numbers constraints

(defn min-val [min] (fn [val] (gen-validation (>= val min) :min min)))
(defn max-val [max] (fn [val] (gen-validation (<= val max) :max max)))
(defn range-val [min max] (fn [val] (gen-validation (or (>= val min) (<= val max)) :range min max)))

(def parse-types
  {Integer    #(Integer/parseInt %)
   Double     #(Double/parseDouble %)
   Boolean    #(Boolean/parseBoolean %)
   Long       #(Long/parseLong %)
   BigInteger #(BigInteger. %)})

(defn check-type
  [value valid-type]
  (if (= (type value) valid-type)
    value
    (try (let [value ((parse-types valid-type) (str value))]
           value)
         (catch Exception e nil))))

(defn type-of
  "Creates a function to validate a field type.
   The passed type can be one of the following classes:
     String
     Boolean
     Integer
     Long
     BigInteger
     Double
   Or a function from a validator definition
   Usage
         (validator
           {:name   (type-of String)
            :address (type-of (validator {:number (type-of Integer)}))})"
  {:no-doc true}
  [valid-type]
  (fn [value required?]
    (if value
      (cond (class? valid-type)
            (let [new-value (check-type value valid-type)]
              (assoc (gen-validation (not (nil? new-value)) :type-error)
                :output new-value :input value))
            (fn? valid-type)
            (valid-type value))
      (if required?
        (gen-validation false :required-error)
        (gen-validation true :no-error)))))

(let [class-fn (fn [valid-type]
                 (fn [[valid-list? input output] value]
                   (let [new-value   (check-type value valid-type)
                         valid-elem? (not (nil? new-value))]
                     [(and valid-elem? valid-list?)
                      (if valid-elem?
                        input
                        (conj input (assoc (gen-validation (not (nil? new-value)) :type-error)
                                      :input value)))
                      (if new-value (conj output new-value) output)])))
      valid-fn (fn [valid-type]
                 (fn [[valid-list? input output] val]
                   (let [{valid-elem? :valid? output-elem :output :as validation} (valid-type val)]
                     [(and valid-elem? valid-list?)
                      (if valid-elem? input (conj input (select-keys validation [:input :errors])))
                      (if valid-elem? (conj output output-elem) output)])))]
  (defn list-of
    "Creates a function to validate a field type of a list.
     The passed type can be one of the following classes:
       String
       Boolean
       Integer
       Long
       BigInteger
       Double
     Or a function from a validator definition
     Usage
           (validator
             {:phones (list-of String)
              :books  (list-of (validator {:year (type-of Integer)}))})"
    {:no-doc true}
    [valid-type]
    (fn [list required?]
      (if (not-empty list)
        (let [[valid? input output]
              (reduce
               (cond (class? valid-type) (class-fn valid-type)
                     (fn? valid-type) (valid-fn valid-type))
               [true [] []]
               list)]
          (assoc (gen-validation valid? :type-error) :output output :input input))
        (if required?
          (gen-validation false :required-error)
          (gen-validation true :no-error))))))

(defn- validate
  [schema value]
  (let [[valid-output error-output]
        (reduce
         (fn [[valid-output error-output]
             [field-name {:keys [type-validator required? constraints]}]]
           (let [{:keys [valid? input output] :as validation}
                 (type-validator (field-name value) required?)]
             (if valid?
               (if output
                 (let [errors (reduce
                               (fn [total-errors constraint]
                                 (let [{:keys [valid? errors]} (constraint output)]
                                   (if valid? total-errors (concat errors total-errors))))
                               []
                               constraints)
                       valid? (empty? errors)]
                   (if valid?
                     [(assoc valid-output field-name output) error-output]
                     [valid-output (assoc error-output field-name {:input input :errors errors})]))
                 [valid-output error-output])
               [(assoc valid-output field-name output)
                (assoc error-output field-name
                       (if (coll? output)
                         input
                         (select-keys validation [:input :errors])))])))
         [{} {}]
         schema)]
    {:valid? (and (not (empty? valid-output)) (empty? error-output))
     :input  error-output
     :output valid-output}))

(defn validator
  "Used to validate body content already parsed with wrap-body-parser middleware.
   Receives a schema indicating type of each field, using type-of o list-of functions.
   Optionaly a map with required fields and other constraints can be passed.
   Calling the returned function results in a map with values:
    - valid? = If the validation was successful
    - input  = Map with errors if valid? is false, each invalid field contains a map with the input and errors.
    - output = Valid map
   Usage:
          (let [v (validator
                    {:name   (type-of String)
                     :phones (list-of String)
                     :books  (list-of (validator {:year (type-of String)}))}
                    {:required    [:name]
                     :constraints {:name [(max-size 30) (min-size 10)]}})]
            (v {:required    [:name]
                :constraints {:name [(max-size 30) (min-size 10)]}}))
           ;; => {:valid? false,
                  :input
                  {:name
                   {:input \"Sebastian\",
                    :errors
                    ({:message \"The length must be at least 10\" , :code :min-size})},
                   :phones
                   [{:input 3,
                     :valid? false,
                     :errors [{:message \"Invalid type\", :code :type-error}]}]},
                  :output {:phones [\"555-555-555\"], :books [{:year 2012}]}}"
  [schema & [{:keys [required constraints]}]]
  (let [required (set required)]
    (fn [body]
      (validate (reduce
                 (fn [schema [k v]]
                   (assoc schema
                     k {:type-validator v
                        :required?      (contains? required k)
                        :constraints    (k constraints)}))
                 {}
                 schema) body))))

;; Filter validation

(declare make-filter)

(def invalid-filter (RipException. {:code :invalid-filter :message "Invalid filter structure"}))

(def ^{:private true} query-fns
  {:eq  (fn [field [val]] {field val})
   :neq (fn [field [val]] {field [pred-not= val]})
   :gt  (fn [field [val]] {field [pred-> val]})
   :lt  (fn [field [val]] {field [pred-< val]})
   :ge  (fn [field [val]] {field [pred->= val]})
   :le  (fn [field [val]] {field [pred-<= val]})
   :rng (fn [field [min max]] (pred-or {field [pred->= min]} {field [pred-<= max]}))
   :lk  (fn [field [val]] {field [pred-like val]})})

(defn- make-join
  "Generates an aliased entity and a clause for the join"
  {:no-doc true}
  [alias ent sub-alias sub-ent]
  (let [rel (get-rel ent sub-ent)
        field #(last (clojure.string/split (val (first %)) #"\""))
        pk (field (:pk rel))
        fk (field (:fk rel))]
    [[(:ent rel) (keyword sub-alias)]
     (apply pred-= (map
                    (fn [[alias field]] (keyword (str (name alias) \. field)))
                    [[alias pk] [sub-alias fk]]))]))

(let [class-validator (fn [c] (fn [x] (if (= (type x) c) x (throw invalid-filter))))]
  (defn- field-cond
    "Generates the clause for a field assuming the input is a vector"
    {:no-doc true}
    [field validator pred]
    (if (or (keyword? validator) (fn? validator) (class? validator)
            (and (vector? validator) (keyword? (first validator))
                 (or (fn? (second validator)) (class? (second validator)))))
      (let [[field validator] (cond
                               (vector? validator)
                               (if (class? (second validator))
                                 [(first validator) (class-validator (second validator))]
                                 validator)
                               (keyword? validator)
                               [validator identity]
                               (class? validator)
                               [field (class-validator validator)]
                               :else [field validator])]
        (reduce
         pred-or
         (map (fn [pred]
                (if (map? pred)
                  (reduce
                   pred-or
                   (map (fn [[f vals]]
                          ((query-fns f) field
                           (map validator (if (vector? vals) vals [vals]))))
                        pred))
                  ((query-fns :eq) field (map validator [pred]))))
              pred)))
      (throw invalid-filter))))

(defn- inner-entity
  "Creates an inner entity based on the validator type"
  {:no-doc true}
  [ent alias validator pred]
  (cond
   (fn? validator) (validator pred ent alias)
   (map? validator) (make-filter ent alias validator pred)
   :else (throw invalid-filter)))

(defn- make-filter
  "Creates a validated pair of values consisting of a where clause, and
   a set of joins, each with the aliased entity, and the join clause"
  {:no-doc true}
  [ent alias fields data]
  (reduce
   (fn [[where joins :as result] [field validator]]
     (if-let [pred (data field)]
       (let [field (keyword (str alias "." (name field)))]
         (cond
          (vector? pred)
          [(if (nil? where)
             (field-cond field validator pred)
             (pred-or where (field-cond field validator pred)))
           joins]
          (map? pred)
          (let [[inner-where inner-joins] (inner-entity ent alias validator pred)]
            [(pred-and where inner-where) (concat joins inner-joins)])
          :else (throw invalid-filter)))
       result))
   [nil []]
   fields))

(defn query-validator
  "Creates a function given a korma entity and maps representing the fields.
   The generated function recives the filter map from the query string and generates
   a where clause and a list of joins. The value of the field can be a class, function,
   or a vector with a keyword and the class or function.
   Usage:
          (query-validator users
            {:name    String
             :address {:city [:address_city String]
                       :street [:address_street String]}
             :books   (query-validator books
                        {:name String
                         :year date-validator})})"
  [rel & fields]
  (fn [data & [parent-ent parent-alias :as child?]]
    (let [ent      (if (and child? (keyword? rel)) (get-rel parent-ent rel) rel)
          rel-name (if (keyword? rel) (name rel) (:name rel))
          alias    (if child? (str rel-name "_" parent-alias) rel-name)
          filter   (make-filter ent alias (apply merge fields) data)
          [where-clause joins] (concat (when child? [(make-join parent-alias parent-ent alias rel)]) filter)]
      (if child?
        [where-clause joins]
        (reduce
         (fn [query [ent clause]]
           (join query ent clause))
         (-> (select* ent)
             (where where-clause))
         joins)))))
