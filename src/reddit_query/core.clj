(ns reddit-query.core
  (:require [clj-http.client :as http]))

(def base-url "http://reddit.com")

(defn get-json
  "Given a reddit URL, constructs an HTTP request for the backing JSON data and
   returns the retrieved data in EDN format."
  [url]
  (-> url
      (str "/.json")
      (http/get {:as :json :query-params {:limit 100}})
      :body))

(defn link-ids
  "Given a link listing URL, returns a seq of link IDs present in the listing."
  [url]
  (->> (get-json url)
       :data :children
       (map :data)
       (map :id)))

(defn comment-threads
  "Given a link ID, returns a seq of comment threads posted in response to the
   corresponding link. Each comment thread is an EDN representation of raw JSON
   data, and can be passed to `comments` to retrieve a cleaner representation
   of the constituent comments."
  [link-id]
  (let [url (str base-url "/comments/" link-id)]
    (->> (get-json url)
         second :data :children
         (map :data))))

(defn comments
  "Given a comments thread, returns a map from comment IDs to individual
   comments in the thread. Each comment is represented by a map with the
   following keys:

     :id      => the comment's unique ID
     :replies => a seq of the unique IDs of direct replies to the comment
     :body    => the comment text
     :ups     => the number of upvotes the comment has received
     :downs   => the number of downvotes the comment has received
     :author  => the username of the comment's author
   
   It is not guaranteed that a comment whose ID appears in the :replies to a
   returned comment will itself be present in the returned map."
  [comment-thread]
  (letfn [(replies [comment]
            (map :data (get-in comment [:replies :data :children])))]
    (->> (tree-seq (comp seq replies) replies comment-thread)
         (map #(assoc % :replies (map :id (replies %))))
         (map #(select-keys % [:id :replies :body :ups :downs :author]))
         (filter :body) ; this is weird, but we get empty comments otherwise
         (reduce #(assoc %1 (:id %2) %2) {}))))

(defn all-comments
  "Given a link ID, returns a map from comment IDs to individual comments,
   potentially containing comments from multiple individual threads."
  [link-id]
  (apply merge (map comments (comment-threads link-id))))
