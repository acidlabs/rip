(ns rip.validation
  "Validation functions for body input and filters for collection queries"
  (:import rip.RipException)
  (:use korma.sql.fns
        korma.core
        rip.db)
  (:require [clojure.string :as st]))

(def ^{:dynamic true} *schema* nil)

;; Body validation

(def ^{:private true} default-messages
  (atom
   {:min-length "The length must be at least %s"
    :max-length "The length must be up to %s"
    :range-length "The length must be between %s and %s"
    :type-error "Invalid type"
    :required-error "This field is required"
    :max "It must be up to %s"
    :min "It must be at least %s"
    :range "It must be between %s and %s"
    :no-error ""}))

(defn update-messages
  [msgs]
  (swap! default-messages merge msgs))

(defn gen-validation
  [test code & msg-params]
  {:valid? test :error {:message (apply format (code @default-messages) msg-params) :code code}})

;; Collections and strings constraints
(defn min-length [min] (fn [val] (gen-validation (>= (count val) min) :min-length min)))
(defn max-length [max] (fn [val] (gen-validation (<= (count val) max) :max-length max)))
(defn range-length [min max] (fn [val] (gen-validation (or (>= (count val) min)
                                                          (<= (count val) :range-length min max)))))

;; Numbers constraints
(defn min-val [min] (fn [val] (gen-validation (>= val min) :min min)))
(defn max-val [max] (fn [val] (gen-validation (<= val max) :max max)))
(defn range-val [min max] (fn [val] (gen-validation (or (>= val min) (<= val max)) :range min max)))

(defn- type-of
  "Create a function to validate a field type."
  {:no-doc true}
  [valid-type & [opts]]
  (let [required? (= opts :required)]
    (fn [val]
      (if val
        (cond (class? valid-type) (assoc (gen-validation (= (type val) valid-type) :type-error)
                                    :output val :input val)
              (fn? valid-type) (valid-type val))
        (if required?
          (gen-validation false :required-error)
          (gen-validation true :no-error))))))

(let [class-fn (fn [valid-type]
                 (fn [[valid-list? input output] val]
                   (let [valid-elem? (= (type val) valid-type)]
                     [(and valid-elem? valid-list?)
                      (if valid-elem? input (conj input
                                                  (assoc (select-keys (gen-validation false :type-error) [:error])
                                                    :input val)))
                      (if valid-elem? (conj output val) output)])))
      valid-fn (fn [valid-type]
                 (fn [[valid-list? input output] val]
                   (let [{valid-elem? :valid? output-elem :output :as validation} (valid-type val)]
                     [(and valid-elem? valid-list?)
                      (if valid-elem? input (conj input (select-keys validation [:input :error])))
                      (if valid-elem? (conj output output-elem) output)])))]

  (defn- list-of
    "Create a function to validate the type of the elements in a list field"
    {:no-doc true}
    [valid-type & [opts]]
    (let [required? (= opts :required)]
      (fn [list]
        (if (not-empty list)
          (let [[valid? input output]
                (reduce (cond (class? valid-type) (class-fn valid-type)
                              (fn? valid-type) (valid-fn valid-type))
                        [true [] []]
                        list)]
            (assoc (gen-validation valid? :type-error) :output output :input input))
          (if required?
            (gen-validation false :required-error)
            (gen-validation true :no-error)))))))

(defn required
  "Used for declaring a required field. The type can be a class, a function or
   a vector containing the class or function to describe a list of that type."
  [type & constraints]
  (let [type-valid (if (vector? type)
                     (list-of (first type) :required)
                     (type-of type :required))]
    (if constraints
      [type-valid constraints]
      [type-valid])))

(defn optional
  "Used for declaring an optional field. The type can be a class, a function or
   a vector containing the class or function to describe a list of that type."
  [type & constraints]
  (let [type-valid (if (vector? type)
                     (list-of (first type))
                     (type-of type))]
    (if constraints
      [type-valid constraints]
      [type-valid])))

(defn body-validator
  "Create a map with values:
    - valid? = If the validation was successful
    - input  = Entity with posible errors with {:input :error} on invalid fields.
    - output = Valid entity map
   Usage:
          (body-validator
            {:name    (required String (max-length 30))
             :phones  (required [String] (min-length 1))
             :books   (optional [(body-validator {:year (required string->date)})])
             :address (optional (body-validator
                                  {:city (required String)
                                   :street (optional String)}))})"
  [& schemas]
  (let [schema (apply merge schemas)]
    (fn [val]
      (let [[valid-output error-output]
            (reduce
             (fn [[valid-output error-output] [field-name validator]]
               (let [[type-valid constraints] (if (fn? validator) [validator] validator)
                     {:keys [valid? input output] :as validation} (type-valid (field-name val))]
                 (if valid?
                   (if output
                     (let [errors (reduce
                                   (fn [errors constraint]
                                     (let [{:keys [valid? error]} (constraint output)]
                                       (if valid? errors (cons error errors))))
                                   []
                                   constraints)
                           valid? (empty? errors)]
                       (if valid?
                         [(assoc valid-output field-name output) error-output]
                         [valid-output (assoc error-output field-name {:input input :error errors})]))
                     [valid-output error-output])
                   [(assoc valid-output field-name output)
                    (assoc error-output field-name
                           (if (coll? output)
                             input
                             (select-keys validation [:input :error])))])))
             [{} {}]
             schema)]
        {:valid? (and (not (empty? valid-output)) (empty? error-output))
         :input error-output
         :output valid-output}))))

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
  (let [rel @((:rel ent) (:name sub-ent))
        field #(last (clojure.string/split (val (first %)) #"\""))
        pk (field (:pk rel))
        fk (field (:fk rel))]
    [[sub-ent (keyword sub-alias)]
     (apply pred-= (map
                    (fn [[alias field]] (keyword (str (name alias) \. field)))
                    [[alias pk] [sub-alias fk]]))]))

(let [class-validator (fn [c] (fn [x] (if (= (type x) c) x (throw invalid-filter))))]
  (defn- field-cond
    "Generates the clause for a field assuming the input corresponds to a vector"
    {:no-doc true}
    [field validator pred]
    (if (or (keyword? validator) (fn? validator) (class? validator)
            (and (vector? validator) (keyword? (first validator))
                 (or (fn? (second validator)) (class? (second validator)))))
      (let [[field validator] (cond (vector? validator) (if (class? (second validator))
                                                          [(first validator) (class-validator (second validator))]
                                                          validator)
                                    (keyword? validator) [validator identity]
                                    (class? validator) [field (class-validator validator)]
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
   keyword (to be replaced for the original field keyword). If a vector is passed,
   the first value must be a keyword and the second a class or function.
   Usage:
          (query-validator user
            {:name    String
             :address {:city [:address_city String]
                       :street [:address_street String]}
             :books   (query-validator books
                        {:name String
                         :year date-validatior})})"
  [ent & fields]
  (fn [data & [parent-ent parent-alias :as child?]]
    (let [ent (set-schema ent *schema*)
          alias (if parent-alias (str (:name ent) "_" parent-alias) (:name ent))
          [where joins] (make-filter ent alias (apply merge fields) data)]
      [where (concat (when child? [(make-join parent-alias parent-ent alias ent)]) joins)])))
