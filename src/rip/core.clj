(ns rip.core
  "Provides a defaction macro for a more RESTFul definition of request handlers."
  (:use [compojure.core]
        [hiccup.util])
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

(defn ->url [url {:keys [path-params query-params]}]
  "Creates a url based on parameters
     :query-params for query string params
     :path-params for common url params"
  (query-url (path-url url path-params) query-params))

(defmacro defaction
  "Define a compojure route with an action vector of the form [method request-handler url-handler],
   also generate a reverse routing function named 'action-name'->url.
   Usage:
         (defaction show-invoice \"/inoices/:id\"
           [:get (fn [{{id :id}:params}] (find-by-id invoices id)) (fn [url] {:show {:href url}})])"
  [name path action]
  (let [method-sym (case (first (eval action))
                     :get #'GET
                     :post #'POST
                     :put #'PUT
                     :delete #'DELETE
                     :options #'OPTIONS
                     :head #'HEAD
                     :patch #'PATCH)]
    `(let [request-handler# (second ~action)
           url-handler# (nth ~action 2)]
       (defn ~(symbol (str name "->url")) [& params#] (url-handler# (->url ~path (first params#))))
       (def ~name
         (~method-sym ~path request# (request-handler# request#)))
       ~name)))
