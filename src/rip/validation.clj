(ns rip.validation
  "Validation functions for body input and filters for collection queries"
  (:import rip.RipException)
  (:use korma.sql.fns
        korma.core
        korma.db
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
   :query-error    "Invalid query"
   :no-error       ""})

(defn gen-validation
  [test code & msg-params]
  {:valid? test
   :$errors [{:message (apply format (code *default-messages*) msg-params)
              :code code}]})

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

(defn- gen-parser
  [value valid-type]
  (let [parser (cond
                (class? valid-type)
                (parse-types valid-type)
                (fn? valid-type)
                valid-type)
        new-value (try ( (str value))
                       (catch Exception e nil))]
    (assoc (gen-validation (if new-value true false) :query-error)
      :$input {}
      :output new-value)))

(defn- check-type
  [value valid-type]
  (if (= (type value) valid-type)
    value
    (try ((parse-types valid-type) (str value))
         (catch Exception e nil))))

(defn- type-of
  {:no-doc true}
  [valid-type]
  (fn [value required?]
    (if value
      (cond (class? valid-type)
            (let [new-value (check-type value valid-type)]
              (assoc (gen-validation (not (nil? new-value)) :type-error)
                :output new-value :$input value))
            (fn? valid-type)
            (valid-type value))
      (if required?
        (gen-validation false :required-error)
        (gen-validation true :no-error)))))

(defn- class-fn
  [valid-type]
  (fn [[valid-list? input output] value]
    (let [new-value   (check-type value valid-type)
          valid-elem? (not (nil? new-value))]
      [(and valid-elem? valid-list?)
       (if valid-elem?
         input
         (conj input (assoc (gen-validation (not (nil? new-value)) :type-error)
                       :input value)))
       (if new-value (conj output new-value) output)])))

(defn- valid-fn
  [valid-type]
  (fn [[valid-list? input output] val]
    (let [{valid-elem? :valid? output-elem :output :as validation} (valid-type val)]
      [(and valid-elem? valid-list?)
       (if valid-elem? input (conj input (select-keys validation [:$input :$errors])))
       (if valid-elem? (conj output output-elem) output)])))

(defn- list-of
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
        (assoc (gen-validation valid? :type-error) :output output :$input input))
      (if required?
        (gen-validation false :required-error)
        (gen-validation true :no-error)))))

(defn- validate
  [schema value]
  (let [[valid-output error-output]
        (reduce
         (fn [[valid-output error-output]
             [field-name {:keys [type-validator required? constraints]}]]
           (let [{:keys [valid? $input output] :as validation}
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
                     [valid-output (assoc error-output field-name {:$input $input :$errors errors})]))
                 [valid-output error-output])
               [(assoc valid-output field-name output)
                (assoc error-output field-name
                       (if (coll? output)
                         valid-output
                         (select-keys validation [:$input :$errors])))])))
         [{} {}]
         schema)]
    {:valid? (and (not (empty? valid-output)) (empty? error-output))
     :input  error-output
     :output valid-output}))

(defn validator
  "Used to validate body content already parsed with wrap-body-parser middleware.
   Receives a schema indicating type of each field.
   Optionaly a map with required fields and other constraints can be passed.
   Calling the returned function results in a map with values:
    - valid? = If the validation was successful
    - input  = Map with errors if valid? is false, each invalid field contains a map with the input and errors.
    - output = Valid map
   Usage:
          (let [v (validator
                    {:name   String
                     :phones [String]
                     :books  [{:year String}]}
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

(def preds
  {:$eq  (fn [field val] {field val})
   :$ne  (fn [field val] {field [pred-not= val]})
   :$gt  (fn [field val] {field [pred-> val]})
   :$lt  (fn [field val] {field [pred-< val]})
   :$ge  (fn [field val] {field [pred->= val]})
   :$le  (fn [field val] {field [pred-<= val]})
   :$lk  (fn [field val] {field [pred-like val]})
   ;;   :rng (fn [field [min max]] (pred-or {field [pred->= min]} {field [pred-<= max]}))
   })

(def ops
  {:$or  pred-or
   :$and pred-and})

(declare validate-filter)

(defn error-field
  [input output field pred]
  [(assoc input field {:$input pred
                       :$error (:query-error *default-messages*)})
   output])

(defn concat-pred
  [pred new-pred & [op]]
  (let [op (or op pred-and)]
    (if-not pred
      new-pred
      (if (and (coll? pred) (empty? pred))
        new-pred
        (op pred new-pred)))))

(defn add-preds
  [input output field [in out]]
  [(if (empty? in)
     input
     (assoc input field in))
   (if (and (coll? output) (empty? output))
     out
     (pred-and output out))])

(defn assoc-op
  [input output field pred op f]
  (add-preds
   input
   output
   field
   (reduce
    (fn [[input output] [in out]]
      [in out]
      [(if (empty? in) input
           (conj input in))
       (concat-pred output out op)])
    [{} false]
    (mapv f pred))))

(defn get-valid?
  [valid-type]
  (let [parser (cond
                (fn? valid-type)
                valid-type
                (class? valid-type)
                (parse-types valid-type))]
    (fn [value]
      (try
        (if (= valid-type String)
          (if (string? value)
            value)
          (parser (str value)))
        (catch Exception e nil)))))

(defn get-alias
  [aliases [node & path]]
  (let [alias (aliases node)
        [alias aliases] (if (string? alias)
                          [alias]
                          alias)]
    (if aliases
      (get-alias aliases path)
      [alias (first path)])))

(declare validate-field)

(defn validate-field-value
  [input output field value valid? alias & [field-name]]
  (let [valid-value (valid? value)]
    (if-not (nil? valid-value)
      [input (concat-pred output
                          (pred-=
                           (str alias
                                "."
                                (if field-name
                                  (name field-name)
                                  (name field))) valid-value))]
      (error-field input output field value))))

(defn validate-field-op
  [input output field value valid? alias op & [field-name]]
  (let [valid-value (valid? value)]
    (if-not (nil? valid-value)
      (if-let [op (preds op)]
        [input (concat-pred output
                            (op
                             (str alias
                                  "."
                                  (if field-name
                                    (name field-name)
                                    (name field))) value))]
        (error-field input output field value))
      (error-field input output field value))))

(defn validate-field-map
  [field pred valid? alias & [field-name]]
  (reduce
   (fn [[input output] [op value]]
     (if-let [op (ops op)]
       (if (vector? value)
         (assoc-op
          input output field value op
          (fn [pred]
            (validate-field input {} field pred valid? alias field-name)))
         (error-field input output field value))
       (validate-field-op input output field value valid? alias op field-name)))
   [{} {}]
   pred))

(defn validate-field
  [input output field pred valid? alias & [field-name]]
  (if (map? pred)
    (add-preds input output field (validate-field-map field pred valid? alias field-name))
    (if-not (vector? pred)
      (validate-field-value input output field pred valid? alias field-name)
      (error-field input output field pred))))

(defn validate-nested
  [input output field pred alias valid-type aliases]
  (if (map? pred)
    (add-preds input output field (validate-filter alias valid-type pred aliases))
    (error-field input output field pred)))

(defn validate-filter
  [alias schema pred aliases]
  (reduce
   (fn [[input output] [field-op pred]]
     (if-let [op (ops field-op)]
       ;; $and $or
       (if (vector? pred)
         (assoc-op
          input output field-op pred op
          (fn [pred]
            (validate-filter alias schema pred aliases)))
         (error-field input output field-op pred))
       ;; field
       (if-let [[valid-type & path] (schema field-op)]
         (if (map? valid-type)
           (let [[alias aliases] (if path
                                   (get-alias aliases path)
                                   [alias aliases])]
             (validate-nested
              input output field-op pred
              alias valid-type aliases))
           (let [[alias aliases] (if-let [[field-or-rel & rest-path] path]
                                   (if rest-path
                                     (get-alias aliases path)
                                     [alias field-or-rel])
                                   [alias aliases])]
             (validate-field
              input output field-op pred
              (get-valid? valid-type) alias
              (if path (name aliases)))))
         [input output])))
   [{} {}]
   pred))

(defn make-join
  "Generates an aliased entity and a clause for the join"
  [rel alias sub-alias join-type]
  (let [field #(last (clojure.string/split (val (first %)) #"\""))
        pk    (field (:pk rel))
        fk    (field (:fk rel))]
    [[(keyword (:name (:ent rel)))
      (keyword sub-alias)
      join-type
      (apply pred-=
             (map
              (fn [[alias field]] (keyword (str (name alias) \. field)))
              [[alias pk] [sub-alias fk]]))]]))

(defn make-joins
  [ent alias joins]
  (reduce
   (fn [[joins aliases] [rel-name join-type]]
     (let [rel       (get-rel ent rel-name)
           sub-alias (str (name rel-name) "_" alias)
           sub-ent   (:ent rel)]
       join-type
       (let [[joins aliases join-type]
             (if (vector? join-type)
               (let [[join-type nested-joins] join-type
                     [j j-a]                  (make-joins sub-ent sub-alias nested-joins)]
                 [(concat joins j)
                  (assoc aliases rel-name [sub-alias j-a])
                  join-type])
               [joins aliases join-type])]
         (let [[join mapping-join] (make-join rel alias sub-alias join-type)]
           [(concat joins [join] (if mapping-join [mapping-join] []))
            (assoc aliases rel-name sub-alias)]))))
   [[] {}]
   joins))

(defn to-query
  [ent joins where-clause]
  (reduce
   (fn [query [name alias type clause]]
     (join query type [name alias] clause))
   (-> (select* ent)
       (where* where-clause))
   joins))

(defn filter-validator
  [ent schema joins]
  (fn [value]
    (let [[joins aliases] (make-joins ent (:name ent) joins)
          [errors where-clause] (validate-filter (:name ent) schema value aliases)]
      {:valid? (and (not (empty? where-clause)) (empty? errors))
       :errors errors
       :query  (to-query ent joins where-clause)})))
