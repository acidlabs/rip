(ns rip.middleware
  (:require [cheshire.core :as json])
  (:require [clojure.data.xml :as xml])
  (:use [remvee.ring.middleware.basic-authentication]))

(defn- json-request?
  [req]
  (if-let [#^String type (:content-type req)]
    (.startsWith type "application/json")))

(defn- xml-request?
  [req]
  (if-let [#^String type (:content-type req)]
    (.startsWith type "application/xml")))

(defn xml->hash-map [{:keys [tag content]}]
  {tag (if (= (type (first content)) clojure.data.xml.Element)
         (reduce (fn [m e]
                   (let [[tag content] (first (xml->hash-map e))]
                     (assoc m tag (if-let [x (m tag)]
                                    (if (vector? x)
                                      (cons x content)
                                      [x content])
                                    content)))) {} content)
         (let [cont (first content)
               val (try (read-string cont)
                        (catch Exception e cont))]
           (try (if (symbol? val) cont val))))})

(defn parse-xml [s] (xml->hash-map (xml/parse-str s)))

(defn wrap-input-param [handler]
  (fn [{:keys [body params] :as req}]
    (if  ((not= (:request-method req) :delete) body)
      (let [bstr (slurp body)
            dto (cond
                  (json-request? req) (json/parse-string bstr true)
                  (xml-request? req) (val (first (parse-xml bstr)))
                  :else bstr)
            req* (assoc req :params (assoc params :input input))]
        (handler req*))
      (handler req))))

(defn authenticated? [name pass] (and (= name "foo") (= pass "bar")))

(def wrap-authentication #(wrap-basic-authentication % authenticated?))

(defn wrap-user [handler]
  (fn [{:keys [body params] :as req}]
    (if body
      (let [bstr (slurp body)
            dto (cond
                  (json-request? req) (json/parse-string bstr true)
                  (xml-request? req) (val (first (parse-xml bstr)))
                  :else bstr)
            req* (assoc req :params (assoc params :dto dto))]
        (handler req*))
      (handler req))))

(defn wrap-utf-8
  "Adds the 'charset=utf-8' clause onto the content type declaration,
  allowing pages to display all utf-8 characters."
  [handler]
  (fn [request]
    (let [resp (handler request)
          ct (get-in resp [:headers "Content-Type"])
          neue-ct (str ct "; charset=utf-8")]
      (assoc-in resp [:headers "Content-Type"] neue-ct))))
