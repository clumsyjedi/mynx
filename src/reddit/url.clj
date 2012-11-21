(ns reddit.url
  (:require [clojure.string :as str]))

(defmacro reddit
  "Macro, turns `(reddit api eg)` into 'http://www,reddit.com/api/eg'"
  [& rest] (str "http://www.reddit.com/" (str/join "/"  rest)))

;; Subreddits can be provided in the form
;; `"subreddit"` or `["sr1" "sr2" ...]`

; Obsolete
; (defn- parse-subreddits [names] (if (string? names) [names] names))

(defn subreddit
  "Links page for a given subreddit(s) (string or vector of strings)."
  [names]
  (let [names (if (string? names) [names] names)]
    (str (reddit r) "/" (str/join "+" names))))

(defn subreddit-comments
  "Comments page url for a given subreddit(s)."
  [names] (str (subreddit names) "/comments"))

(defn subreddit-new
  "New links page url for a given subreddit(s)."
  [names] (str (subreddit names) "/new"))

(defn user [username]
  (str (reddit u) "/" username))
