(ns rip.test.core
  (:use [rip.core])
  (:use [clojure.test]))

(comment
  (def app
    (-> #'root
        (wrap-server-error (fn [e] {:status 500
                                   :headers {"Content-Type" "text/html"}
                                   :body (str "Error: " e)}))
        handler/api
        wrap-input-param
        (wrap-reload ["src"]))))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))
