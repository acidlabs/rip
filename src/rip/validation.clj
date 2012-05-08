(ns rip.validation
  (:require [clojure.string :as st]))

(def messages
  {:min-length "wea wea"
   :type-error "Error de tipo"
   :required-error "Null error"
   :no-error ""
   :schema-error "dsfsdf"})

(defn gen-validation
  [test code & msg-params]
  {:valid? test :error {:message (apply format (code messages) msg-params) :code code}})

(defn min-length [min] (fn [val] (gen-validation (>= (count val) min) :min-length min)))
(defn max-length [max] (fn [val] (gen-validation (<= (count val) :max-length max))))
(defn range-length [min max] (fn [val] (gen-validation (or (>= (count val) min) (<= (count val) :range-length min max)))))

(defn min [min] (fn [val] (gen-validation (>= val min) :min min)))
(defn max [max] (fn [val] (gen-validation (<= val max) :max max)))
(defn range [min max] (fn [val] (gen-validation (or (>= val min) (<= val max)) :range min max)))

(defn rut [] (fn [val] (assoc (gen-validation (nil? (re-find #"" val)) :rut-error) :output (st/split "142512521-5" #"-"))))

(defn type-of
  [valid-type & [opts]]
  (let [required? (= opts :required)]
    (fn [field val]
      (if val
        (cond (class? valid-type) (assoc (gen-validation (= (type val) valid-type) :type-error) :output val :input val)
              (fn? valid-type) (valid-type field val))
        (if required?
          (gen-validation false :required-error)
          (gen-validation true :no-error))))))

(defn list-of
  [valid-type & [opts]]
  (let [required? (= opts :required)]
    (fn [field list]
      (if list
        (cond (class? valid-type) (map (fn [val]
                                         (assoc (gen-validation (= (type val) valid-type) :type-error) :output val :input val))
                                       list)
              (fn? valid-type) (map valid-type list))
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
                               (fn [errors valid]
                                 (let [{:keys [valid? error]} (valid output)]
                                   (if valid? errors (cons error errors))))
                               []
                               constraints)
                       valid? (empty? errors)]
                   (if valid?
                     [(assoc valid-output field-name output) error-output]
                     [valid-output (assoc error-output field-name {:input input :error errors})]))
                 [valid-output error-output])
               [(assoc valid-output field-name output) (assoc error-output field-name
                                                              (if (map? output)
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
     [value#] (defvalidator* ~(keyword (str name)) value# ~fields)))

(defvalidator phone
  {:numero [(type-of Long :required)]})

(def user-schema
  {:nombre [(type-of Boolean :required)]
   :telefono [(type-of phone :required)]
   :clave  [(type-of String :required) (min-length 3)]})

(defvalidator user
  user-schema)

(user
 {:nombre true
  :clave "aaasdasd"
  :basura "SDFsdfsdf"
  :telefono {:numero :numero}
  234 234})
