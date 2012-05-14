(ns rip.validation
  (:require [clojure.string :as st]))

(def ^{:private true} messages
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

(defn default-messages
  [msgs]
  (swap! messages (fn [_] msgs)))

(defn gen-validation
  [test code & msg-params]
  {:valid? test :error {:message (apply format (code @messages) msg-params) :code code}})

;; Collections constraints
(defn min-length [min] (fn [val] (gen-validation (>= (count val) min) :min-length min)))
(defn max-length [max] (fn [val] (gen-validation (<= (count val) max) :max-length max)))
(defn range-length [min max] (fn [val] (gen-validation (or (>= (count val) min) (<= (count val) :range-length min max)))))

;; Numbers constraints
(defn min [min] (fn [val] (gen-validation (>= val min) :min min)))
(defn max [max] (fn [val] (gen-validation (<= val max) :max max)))
(defn range [min max] (fn [val] (gen-validation (or (>= val min) (<= val max)) :range min max)))

(defn type-of
  "Create a function to validate a field type"
  [valid-type & [opts]]
  (let [required? (= opts :required)]
    (fn [field val]
      (if val
        (cond (class? valid-type) (assoc (gen-validation (= (type val) valid-type) :type-error) :output val :input val)
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
  (defn list-of
    "Create a function to validate the type of the elements in a list field"
    [valid-type & [opts]]
    (let [required? (= opts :required)]
      (fn [field list]
        (if list
          (let [[valid? input output]
                (reduce (cond (class? valid-type) (class-fn valid-type)
                              (fn? valid-type) (valid-fn valid-type))
                        [true [] []]
                        list)]
            (assoc (gen-validation valid? :type-error) :output output :input input))
          (if required?
            (gen-validation false :required-error)
            (gen-validation true :no-error)))))))

(defn- defvalidator*
  "Create a valid entity(if posible) and error entity with {:input :error} on invalid fields"
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
  "Generate a function to validate an input entity"
  [name fields]
  `(defn ~name
     [value#] (defvalidator* value# ~fields)))

(comment
  "Improved type definition example"
  (defvalidator cosa
   {:nombre [String not-null (max 10)]
    :telefonos [[telefono] not-null (min-length 1)]}))
