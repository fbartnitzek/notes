(defn create
  []
  (atom {}))

(defn get
  [cache key]
  (@cache key))

; 2 different put functions: key-value + map
(defn put
  ([cache value-map]
   (swap! cache merge value-map))
  ([cache key value]
   (swap! cache assoc key value))
)