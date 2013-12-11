(ns reddit.util
  (use chiara
       chiara.threading))

(use-chiara) (chiara

(defn filter-map [f map]
  (into {} (filter f map)))

(defn memoize'
  "Like the original memoize, except that results expire
  after t milliseconds."
  [f timeout]
  (let [mem (atom {})]
    (fn [& args]
      (swap! mem #(filter-map
                    (fn [[_ [_ t]]]
                      (< (- (system-ms) t)
                         timeout))
                    %))
      (if-let [[result _] (get @mem args)]
        result
        (let [result (apply f args)]
          (swap! mem assoc args [result (system-ms)])
          result)))))

(defn apply-opts [f & args]
  (let [opts (last    args)
        args (butlast args)]
    (apply f (concat args (reduce concat opts)))))

let [xs (atom [nil #{}])]
  defn repeat? [x]
    first
      swap! xs
        λ [[_ xs]]
          if (xs x)
            [true xs]
            [false (conj xs x)]

)
