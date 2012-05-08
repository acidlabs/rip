(ns rip.validation
  (:require [clojure.string :as st]))

(def messages
  {:min-length "wea wea"
   :max-length "wea wea"
   :type-error "Error de tipo"
   :required-error "Null error"
   :no-error ""
   :schema-error "dsfsdf"})

(defn gen-validation
  [test code & msg-params]
  {:valid? test :error {:message (apply format (code messages) msg-params) :code code}})

;; Collections constratints
(defn min-length [min] (fn [val] (gen-validation (>= (count val) min) :min-length min)))
(defn max-length [max] (fn [val] (gen-validation (<= (count val) max) :max-length max)))
(defn range-length [min max] (fn [val] (gen-validation (or (>= (count val) min) (<= (count val) :range-length min max)))))

;; Numbers constratints
(defn min [min] (fn [val] (gen-validation (>= val min) :min min)))
(defn max [max] (fn [val] (gen-validation (<= val max) :max max)))
(defn range [min max] (fn [val] (gen-validation (or (>= val min) (<= val max)) :range min max)))

(defn type-of
  [valid-type & [opts]]
  (let [required? (= opts :required)]
    (fn [field val]
      (if val
        (cond (class? valid-type) (assoc (gen-validation (= (type val) valid-type) :type-error) :output val :input val)
              (fn? valid-type) (valid-type val))
        (if required?
          (gen-validation false :required-error)
          (gen-validation true :no-error))))))

(defn list-of
  [valid-type & [opts]]
  (let [required? (= opts :required)]
    (fn [field list]
      (if list
        (let [[valid? input output]
              (reduce
               (cond (class? valid-type)
                     (fn [[valid-list? input output] val]
                       (let [valid-elem? (= (type val) valid-type)]
                         [(and valid-elem? valid-list?)
                          (if valid-elem? input (conj input
                                                      (assoc (select-keys (gen-validation false :type-error) [:error])
                                                        :input val)))
                          (if valid-elem? (conj output val) output)]))
                     (fn? valid-type)
                     (fn [[valid-list? input output] val]
                       (let [{valid-elem? :valid? output-elem :output :as validation} (valid-type val)]
                         [(and valid-elem? valid-list?)
                          (if valid-elem? input (conj input (select-keys validation [:input :error])))
                          (if valid-elem? (conj output output-elem) output)])))
               [true [] []]
               list)]
          (assoc (gen-validation valid? :type-error) :output output :input input))
        (if required?
          (gen-validation false :required-error)
          (gen-validation true :no-error))))))

(defn- defvalidator*
  "Creates a valid entity(if posible) and a list of accumulated errors"
  [val schema]
  (let [[valid-output error-output]
        (reduce
         (fn [[valid-output error-output] [field-name [type-valid & constraints]]]
           (let [{:keys [valid? input output] :as validation} (type-valid field-name (field-name val))]
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
               [(assoc valid-output field-name output) (assoc error-output field-name
                                                              (if (coll? output)
                                                                input
                                                                (select-keys validation [:input :error])))])))
         [{} {}]
         schema)]
    {:valid? (and (not (empty? valid-output)) (empty? error-output))
     :input error-output
     :output valid-output}))

(defmacro defvalidator
  [name fields]
  `(defn ~name
     [value#] (defvalidator* value# ~fields)))

(defvalidator phone
  {:numero [(type-of Long :required)]})

(def user-schema
  {:nombre [(type-of Boolean :required)]
   :telefono [(type-of phone :required)]
   :telefonos [(list-of phone :required)]
   :clave  [(type-of String :required) (min-length 3)]})

(defvalidator user
  user-schema)

(user
 {:nombre true
  :clave "aaa"
  :basura "SDFsdfsdf"
  :telefono {:numero 12432}
  :telefonos [{:numero 234234}]})
