(ns rip.core
  "Provides a defaction macro for a more RESTFul definition of request handlers."
  (:use compojure.core
        hiccup.util)
  (:require [clojure.string :as string]))

(defn- throwf [msg & args]
  (throw (Exception. (apply format msg args))))

(defn- ^{:skip-wiki true} route-arguments
  "returns the list of route arguments in a route"
  [route]
  (let [args (re-seq #"/(:([^\/\.]+)|\*)" route)]
    (set (map #(keyword (or (nth % 2) (second %))) args))))

(defn- path-url [url route-args]
  (let [url (if (vector? url) ;;handle complex routes
              (first url)
              url)
        route-arg-names (route-arguments url)]
    (when-not (every? (set (keys route-args)) route-arg-names)
      (throwf "Missing route-args %s" (vec (filter #(not (contains? route-args %)) route-arg-names))))
    (reduce (fn [path [k v]]
              (if (= k :*)
                (string/replace path "*" (str v))
                (string/replace path (str k) (str v))))
            url
            route-args)))

(defn- query-url [uri params]
  (str (url uri params)))

(defn ->url [url {:keys [route-params query-params]}]
  "Creates a url based on parameters
     :query-params for query string params
     :route-params for route url params"
  (query-url (path-url url route-params) query-params))

(defn make-method
  [meth]
  (case meth
    :get #'GET
    :post #'POST
    :put #'PUT
    :delete #'DELETE
    :options #'OPTIONS
    :head #'HEAD
    :patch #'PATCH))

(defmacro defaction
  "Define a compojure route with an action vector of the form [method request-handler url-handler],
   also generate a reverse routing function named 'action-name'->url.
   Usage:
         (defaction show-invoice \"/inoices/:id\"
           [:get (fn [{{id :id}:params}] (find-by-id invoices id)) (fn [url] {:show {:href url}})])"
  [name path action]
  (let [method-sym (make-method (first (eval action)))]
    `(let [request-handler# (second ~action)
           url-handler# (nth ~action 2)]
       (defn ~(symbol (str name "->url")) [& params#] (url-handler# (->url ~path (first params#))))
       (def ~name
         (~method-sym ~path request# (request-handler# request#)))
       ~name)))

(defmacro defresource
  "Define a compojure route like defroute and a reverse routing function for each given action.
   Usage:
         (defresource user \"users/:id\"
           show   [\"\" [:get identity identity]]
           books  [\"/books\" [:get identity identity]])"
  [name path & actions]
  (for [[k x] (apply hash-map actions)]
    (let [[action-path [method request-handler url-handler]] (eval x)]
      `(let [url-handler# (last (nth ~x 1))]
         (defn ~(symbol (str name "-" k "->url"))
           [& params#]
           (url-handler# (->url ~(str path action-path) (first params#)))))))
  (cons #'defroutes
        (cons name
              (map (fn [[_ x]]
                     (let [[action-path [method]] (eval x)
                           method-sym (make-method method)]
                       `(let [request-handler# (nth (nth ~x 1) 1)]
                          (~method-sym ~(str path action-path) req# (request-handler# req#)))))
                   (apply hash-map actions)))))
