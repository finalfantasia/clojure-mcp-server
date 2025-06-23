(ns clojure-mcp.tools.scratch-pad.smart-path
  "Smart path operations that automatically convert string indices to numbers for vectors
   and intelligently initialize data structures.")

;; Helper functions

(defn parse-positive-int
  "Parse string to positive integer, returns nil if not valid"
  [s]
  (when (string? s)
    (try
      (let [n (Integer/parseInt s)]
        (when (>= n 0) n))
      (catch Exception _ nil))))

(defn negative-number-string?
  "Check if string represents a negative number"
  [s]
  (boolean (and (string? s) (re-matches #"-\d+" s))))

(defn validate-vector-key
  "Validates a key for vector operations. Throws if invalid."
  [k path-so-far]
  (cond
    (number? k) k
    (parse-positive-int k) (parse-positive-int k)
    (negative-number-string? k)
    (throw (ex-info "Negative indices not allowed for vectors"
                    {:path path-so-far :key k}))
    :else
    (throw (ex-info "String keys not allowed for vectors"
                    {:path path-so-far :key k}))))

(defn normalize-key-for-type
  "Normalizes a key based on data type without validation.
   Returns [normalized-key is-index?]"
  [current-data k]
  (let [parsed-int (parse-positive-int k)]
    (cond
      ;; Already a number
      (number? k) [k (vector? current-data)]

      ;; Valid string index for vector
      (and (vector? current-data) parsed-int) [parsed-int true]

      ;; String that could be an index but data isn't vector
      :else [k false])))

(defn initializing-vector-at-zero?
  "Check if we're initializing a new vector at index 0"
  [current-data k]
  (and (nil? current-data)
       (or (= k "0") (= k 0))))

(defn invalid-vector-initialization?
  "Check if trying to initialize vector at non-zero index"
  [current-data k]
  (and (nil? current-data)
       (let [parsed (parse-positive-int k)]
         (or (and (number? k) (> k 0))
             (and parsed (> parsed 0))))))

(defn normalize-path-key
  "Normalizes a path key based on current data type and operation.
   Returns [normalized-key is-index?] tuple.
   Throws for invalid vector operations."
  [current-data k op path-so-far]
  (cond
    ;; GET operations - lenient
    (= op :get)
    (normalize-key-for-type current-data k)

    ;; DISSOC operations need special handling for vectors
    (= op :dissoc)
    (cond
      (vector? current-data)
      [(validate-vector-key k path-so-far) true]
      :else
      [k false])

    ;; Vector write operations - strict validation
    (vector? current-data)
    [(validate-vector-key k path-so-far) true]

    ;; Special case: initializing new vector at index 0
    (initializing-vector-at-zero? current-data k)
    [0 true]

    ;; Error: trying to initialize vector at non-zero index
    (invalid-vector-initialization? current-data k)
    (throw (ex-info "Cannot initialize vector with non-zero index"
                    {:path path-so-far :key k}))

    ;; Default: use key as-is for maps/other types
    :else
    [k false]))

(defn vector-index-out-of-bounds?
  "Check if vector index is out of bounds for write operations"
  [data index]
  (or (< index 0) (> index (count data))))

(defn validate-vector-bounds
  "Validate vector bounds for write operations. Throws if out of bounds."
  [data index path-so-far]
  (when (vector-index-out-of-bounds? data index)
    (throw (ex-info "Vector index out of bounds"
                    {:path path-so-far
                     :index index
                     :vector-size (count data)}))))

(defn determine-next-structure
  "Determines what structure to create based on the next path element"
  [next-k path-so-far]
  (cond
    (or (= "0" next-k) (= 0 next-k))
    []

    (or (and (number? next-k) (> next-k 0))
        (and (string? next-k)
             (when-let [n (parse-positive-int next-k)]
               (> n 0))))
    (throw (ex-info "Cannot initialize vector with non-zero index"
                    {:path path-so-far :key next-k}))

    :else
    {}))

;; Core operations

(defn apply-operation
  "Apply the operation at the end of the path"
  [data op args]
  (case op
    :get data
    :assoc (first args)
    :update (apply (first args) data (rest args))))

(defn should-create-structure?
  "Check if we should auto-create a structure"
  [current-val remaining-path op]
  (and (nil? current-val)
       (seq remaining-path)
       (not= op :get)))

(defn process-single-key
  "Process a single key in the path"
  [current-data k remaining-path path-so-far op]
  (let [[fixed-k index?] (normalize-path-key current-data k op path-so-far)]

    ;; Validate bounds for vector writes
    (when (and index? (vector? current-data) (not= op :get))
      (validate-vector-bounds current-data fixed-k path-so-far))

    (let [current-val (get current-data fixed-k)]
      {:fixed-k fixed-k
       :current-val current-val
       :next-structure (when (should-create-structure? current-val remaining-path op)
                         (determine-next-structure (first remaining-path)
                                                   (conj path-so-far fixed-k)))})))

(defn process-path
  "Process a path through a data structure with smart conversions"
  [current-data remaining-path path-so-far op args]
  (if (empty? remaining-path)
    (apply-operation current-data op args)

    (let [k (first remaining-path)
          remaining (rest remaining-path)
          {:keys [fixed-k current-val next-structure]}
          (process-single-key current-data k remaining path-so-far op)

          ;; Recurse with the value or new structure
          new-val (process-path (or current-val next-structure)
                                remaining
                                (conj path-so-far fixed-k)
                                op
                                args)]

      ;; Only update structure for non-get operations
      (if (= op :get)
        new-val
        (assoc current-data fixed-k new-val)))))

(defn smart-path-op
  "Core function for path operations with automatic string->index conversion."
  [data path op & args]
  (process-path data path [] op args))

;; Public API

(defn smart-get-in
  "Like get-in but converts string numbers to indices for vectors"
  [data path]
  (smart-path-op data path :get))

(defn smart-assoc-in
  "Like assoc-in with smart conversions and vector initialization"
  [data path value]
  (smart-path-op data path :assoc value))

(defn smart-update-in
  "Like update-in with smart conversions"
  [data path f & args]
  (apply smart-path-op data path :update f args))

(defn smart-dissoc-in
  "Like dissoc-in but handles string->index conversion with validation"
  [data path]
  (if (empty? path)
    data
    (let [k (first path)
          path-so-far (vec (butlast path))]

      (if (= 1 (count path))
        ;; Last element - validate and remove it
        (let [[fixed-k _] (normalize-path-key data k :dissoc path-so-far)]
          (if (vector? data)
            (vec (concat (subvec data 0 fixed-k)
                         (when (< (inc fixed-k) (count data))
                           (subvec data (inc fixed-k)))))
            (dissoc data fixed-k)))
        ;; Recurse
        (let [[fixed-k _] (normalize-path-key data k :dissoc path-so-far)]
          (if-let [sub-data (get data fixed-k)]
            (assoc data fixed-k (smart-dissoc-in sub-data (rest path)))
            data))))))
