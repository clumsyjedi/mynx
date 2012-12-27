(ns reddit
  "High level interface to reddit."
  (:use      reddit.core util.spacers)
  (:require [clojure.string :as str ]
            [cheshire.core  :as json]
            [reddit.url :refer (reddit)]))

(def api-spacer (spacer 2000))
(defmacro api-call
  "Code blocks wrapped with `api-call` will not execute within
  two seconds of each other, eg
      `(pmap #(api-call (println %)) (range 5))`
  takes 8 s, with 2 s between each call to `println`. All calls
  to the reddit api should use this macro."
  [& forms] `(spaced api-spacer ~@forms))

;; --------------
;; Authentication
;; --------------

(defmacro with-user-agent
  [agent & code]
  `(binding [*user-agent* ~agent] ~@code))

(defn login
  "Returns a login object {:name :cookie :modhash}
  for passing to the request functions. If login
  fails, it will contain an :errors key."
  [user pass]
  (let [response (post (reddit api login)
                       :params {"user" user, "passwd" pass, "api_type" "json"})
        {:keys [errors data] :as response-json}
                 (-> response :body (json/decode true) :json)]
    (cond
      ; Successful
      (data :modhash) {:name    user
                       :cookies (response :cookies)
                       :modhash (data :modhash)}
      ; Unsuccessful
      (seq errors)    response-json
      :else           {:errors  :unknown
                       :reponse response
                       :data    response-json})))

(defn login-success?
  "If the login was successful, returns it.
  Otherwise nil."
  [login]
  (if (login :modhash) login))

;; ----------------
;; Retreiving Items
;; ----------------

(defn items
  "Returns a lazy sequence of all items at the given
  url, including subsequent pages. API calls spaced."
  [url & {:keys [params]}]
  (lazy-seq
    (let [s (api-call (get-parsed url :params (merge {:limit 1000
                                                      :sort  "new"}
                                                     params)))]
      (if-not (empty? s)
        (concat s (items url :params (assoc params :after (-> s last :name))))))))

(defn take-while-chunked [pred coll]
  (lazy-seq
    (let [first (filter pred (take 10 coll))
          rest  (drop 10 coll)]
      (when-let [s (seq first)]
        (concat s (take-while-chunked pred rest))))))

(defn items-since
  "Takes all `items` posted after the specified Date."
  [date url] (take-while-chunked #(.after (% :time) date) (items url)))

;; --------------
;; Links/comments
;; --------------

;; # Inspection

(defn comment? [thing] (= (:kind thing) :comment))
(defn link?    [thing] (= (:kind thing) :link   ))

(defn author? [thing user] (= (thing :author) user))

(defn deleted-comment? [comment]
  (and (author? comment   "[deleted]")
       (= (comment :body) "[deleted]")))

(defn x-post?
  "Checks the link title for \"x-post\"
  (and variants)."
  [link] (re-find #"(?i)x-?post|cross-?post" (link :title)))

;; # Retrieval

(defn get-link
  "Return a link object from the given permalink.
  Includes comments on the link as `:replies`."
  [url]
  (let [data     (get-parsed url)
        link     (ffirst data)
        comments (second data)]
    (assoc link :replies comments)))

(def get-comments
  "Retreive comments from a url (a link page)."
  (comp :replies get-link))

(def get-comment
  "Return a comment for the given permalink.
  Currently ignores context param."
  (comp first get-comments))

(defn with-replies
  "Reload the comment/link (e.g. from `items`)
  with :replies data. Only loads one page."
  [thing]
  (let [data     (-> thing :permalink get-parsed)
        link     (ffirst data)
        comments (second data)]
    (cond
      (comment? thing) (-> thing (merge (first comments)))
      (link?    thing) (-> thing (merge link) (assoc :replies comments))
      :else            thing)))

;; # Actions

(defn reply
  "Parent should be a link/comment object, reply is a string.
  Returns a keyword indicating either successfully `:submitted`
  or an error."
  [parent reply login]
  (let [response (post (reddit api comment)
                       :login login
                       :params {:thing_id (parent :name)
                                :text     reply})]
    (condp re-find (response :body)
      #"contentText"                         :submitted
      #".error.RATELIMIT.field-ratelimit"    :rate-limit
      #".error.USER_REQUIRED"                :user-required
      #".error.DELETED_COMMENT.field-parent" :parent-deleted
      (response :body))))

(defn vote
  "Vote :up, :down, or :none on a link/comment."
  [item direction login]
  (post (reddit api vote)
        :login  login
        :params {:id  (item :name)
                 :dir (direction
                        {:up 1, :none 0, :down -1})}))

;; -----
;; Users
;; -----

(defn me
  "Data about the currently logged in user
  from `/api/me.json`."
  [login] (get-parsed (reddit api me)
                      :login login))

(defn get-user
  "Account information for a user."
  [username]
  (get-parsed (str (reddit user) "/" username "/about")))

(defn user-comments
  "Lazy seq of all comments by a user.
  Can take params e.g.
      (user-comments \"user\" :params {:sort \"top\"
                                       :t    \"all\"})"
  [username & opts]
  (apply items (str (reddit user) "/" username "/comments") opts))

(defn by-subreddit
  "Filter comments by subreddit/set of subreddits."
  [comments subreddits]
  (let [subreddits (if (set? subreddits) subreddits #{subreddits})]
    (filter (comp subreddits :subreddit) comments)))
