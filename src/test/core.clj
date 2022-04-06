(ns test.core
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [compojure.core :refer [GET POST DELETE defroutes]]
            [ring.util.response :refer [response]]
            [ring.middleware.json :as middleware]
            [ring.middleware.cors :refer [wrap-cors]]
            [clojure.java.jdbc :as j]
            [clojure.string :refer [index-of]]
  )
)

(def pg-db {:dbtype   "postgresql"
            :dbname   "booknode"
            :host     "localhost"
            :user     "postgres"
            :password "2921"
           }
)

(defn get-result []
  (j/query pg-db
           ["select * from book"]
  )
)

(defn book-filter [key-word, data]
   (filter #(= (key-word %) data) (get-result))
)

(defn search-book [key-word, data]
  (filter (fn [x]
            (not= (index-of (clojure.string/lower-case (get x key-word)) data) nil)) (get-result)
  )
)

(defn generate-random-index [indexes-count indexes-set]
  (if (<= indexes-count 5)
    indexes-set
    (let [new-indexes (conj indexes-set (rand-int indexes-count))]
      (if (= (count new-indexes) 5)
        new-indexes
        (recur indexes-count new-indexes)
      )
    )
  )
)

(defn get-books-by-indexes [indexes books]
  (for [x indexes] (nth books x))
)

(defn random-books [books]
  (let [books-indexes (generate-random-index (count books) #{})]
    (get-books-by-indexes books-indexes books)
  )
)

(defn page-404 [request]
  {:status 404
   :headers {"content-type" "text/plain"}
   :body "Page not found"
  }
)

(defn get-book-by-id [id]
  (first (book-filter :id (Integer/parseInt id)))
)

(defn page-book [request]
  (let [book (-> request :params :id get-book-by-id)]
    book
  )
)

(defn all-books []
  (let [books (get-result)]
    books
  )
)

(defn year-filter [year]
  (if (not= year "")
    (book-filter :year (Integer/parseInt year))
    (get-result)
  )
)

(defn sorting [key type books]
  (case type
  "up"  (sort-by key < books)
  "down" (sort-by key > books)
  )
)

(defn searching-sort [key type books]
  (case key
  "" books
  "price" (sorting :price type books)
  "year"  (sorting :year type books)
  )
)

(defn searching [req]
  (let [action (req :action)]
    (case action
          "title" (searching-sort (req :key) (req :type) (search-book :title (req :content)))
          "author" (searching-sort (req :key) (req :type) (search-book :author (req :content)))
          "year" (searching-sort (req :key) (req :type) (year-filter (req :content)))
          "genre" (searching-sort (req :key) (req :type) (search-book :genres (req :content)))
    )
  )
)

(defn search-handler [request]
  (let [books (searching request)]
    books
  )
)

(defn create-book [request]
  (let [title (request :title)
        author (request :author)
        year (request :year)
        description (request :description)
        price (request :price)
        genres (request :genres)
       ]
       (j/insert! pg-db :book {:title title, :author author, :year year, :description description, :price price, :genres genres})
  )
)

(defn delete-book [request]
  (let [id (-> request :params :id)]
    (j/delete! pg-db :book ["id = ? " (Integer/parseInt id) ])
  )
)

(defroutes app-routes
  (GET "/library" request (response (all-books)))
  (POST "/search" {body :body} (search-handler body))
  (GET "/random-books" request (response (random-books (get-result))))
  (GET "/book/:id" request (response (page-book request)))
  (POST "/create-book" {body :body} (response (create-book body)))
  (DELETE "/delete-book/:id" request (response (delete-book request)))
  page-404
)

(def app (->
           app-routes
           (wrap-cors :access-control-allow-origin [#".*"]
                      :access-control-allow-methods [:get :put :post :delete])
           (middleware/wrap-json-body {:keywords? true :bigdecimals? true})
           (middleware/wrap-json-response)
         )
)

(defn -main []
  (run-jetty app {:port 8085 :join? true})
)


