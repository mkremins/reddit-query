(ns reddit-query.core
  (:require [clj-http.client :as http]
            [clojure.string :as string]))

(def base-url "http://www.reddit.com")

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

(defn- parent [comment]
  (let [[kind id] (string/split (:parent_id comment) #"_")]
    (when (= kind "t1") id)))

(defn- elide-extra-data [comment]
  (select-keys comment [:id :parent :replies :body :ups :downs :author]))

(defn- index-by
  "Given a function `f` and a collection `coll`, returns a map whose values are
   the items from `coll` and whose keys are the values of `(f item)` for each
   item. Assumes that `(f item)` will return a different value for each item."
  [f coll]
  (reduce #(assoc %1 (f %2) %2) {} coll))

(defn fill-missing-comments
  "Given a comments map (of the form returned by `reddit-query.core/comments`)
   and a link ID, locates up to 20 comment IDs for which no data is present in
   the comments map and retrieves data for the corresponding comments. Returns
   a copy of the comments map with the retrieved comment data merged in."
  [comments link-id]
  (let [missing-ids
        (->> (vals comments)
             (mapcat :replies)
             (remove (partial contains? comments))
             (take 20)
             (string/join ","))
        missing-comments
        (-> (str base-url "/api/morechildren.json")
            (http/post {:as :json
                        :query-params {:children missing-ids
                                       :link_id (str "t3_" link-id)}})
            (get-in [:body :jquery 14 3 0]))
        missing-comments
        (->> missing-comments
             (map :data)
             (map #(assoc % :parent (parent %)))
             (map elide-extra-data)
             (index-by :id))]
    (merge comments missing-comments)))

(defn comments
  "Given a comments thread, returns a map from comment IDs to individual
   comments in the thread. Each comment is represented by a map with the
   following keys:

     :id      => the comment's unique ID
     :parent  => the parent comment's unique ID, or nil if top-level
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
         (filter :body) ; this is weird, but we get empty comments otherwise
         (map #(assoc % :replies (map :id (replies %))))
         (map #(assoc % :parent (parent %)))
         (map elide-extra-data)
         (index-by :id))))

(defn all-comments
  "Given a link ID, returns a map from comment IDs to individual comments,
   potentially containing comments from multiple individual threads."
  [link-id]
  (apply merge (map comments (comment-threads link-id))))
