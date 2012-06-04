(ns rip.middleware
  (:require [cheshire.core :as json])
  (:require [clojure.data.xml :as xml]))

(defn- json-request?
  [req]
  (if-let [#^String type (:content-type req)]
    (.startsWith type "application/json")))

(defn- xml-request?
  [req]
  (if-let [#^String type (:content-type req)]
    (.startsWith type "application/xml")))

(defn xml->hash-map
  "USES read-string FUNCTION TO CONVERT INTO CLOJURE TYPES!!"
  [{:keys [tag content]}]
  {tag (if (= (type (first content)) clojure.data.xml.Element)
         (if (= tag :lista)
           (reduce (fn [m e]
                     (let [[tag content] (first (xml->hash-map e))]
                       (cons content m)))
                   []
                   content)
           (reduce (fn [m e]
                     (let [[tag content] (first (xml->hash-map e))]
                       (assoc m tag content)))
                   {}
                   content))
         (let [cont (first content)
               val (try (read-string cont)
                        (catch Exception e cont))]
           (if (symbol? val)
             cont
             (if (= java.lang.Long (type val))
               (try (int val) (Integer. val) (catch Exception e val))
               val))))})

(defn parse-xml [s] (xml->hash-map (xml/parse-str s)))

(defn wrap-input-param [handler]
  (fn [{:keys [body params request-method] :as req}]
    (let [bstr (slurp body)
          input (cond
                  (json-request? req) (json/parse-string bstr true)
                  (xml-request? req) (val (first (parse-xml bstr)))
                  :else bstr)]
      (handler (assoc req :input input)))
    (handler req)))

(defn wrap-server-error
  "Wrap a handler such that exceptions are handled with the given function"
  [handler f]
  (fn [request]
    (try
      (handler request)
      (catch Exception ex
        (f ex)))))
