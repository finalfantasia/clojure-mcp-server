(ns clojure-mcp.sexp.match
  (:require [rewrite-clj.zip :as z]))

(defn match-sexpr
  "Return true if `pattern` matches `data`. 
   Wildcards in `pattern`:
     - `_?` consumes exactly one form
     - `_*` consumes zero or more forms, but if there are more pattern elements
       after it, it will try to align them with the tail of `data`."
  [pattern data]
  (cond
    ;; both are sequences ⇒ walk with possible '_*' backtracking
    (and (sequential? pattern) (sequential? data))
    (letfn [(match-seq [ps ds]
              (cond
                ;; pattern exhausted ⇒ only match if data also exhausted
                (empty? ps)
                (empty? ds)

                ;; '_?' ⇒ must have at least one ds, then consume exactly one
                (= (first ps) '_?)
                (and (seq ds)
                     (recur (rest ps) (rest ds)))

                ;; '_*' ⇒ two cases:
                ;; 1) no more pattern ⇒ matches anything
                ;; 2) with remaining pattern, try every split point
                (= (first ps) '_*)
                (let [ps-rest (rest ps)]
                  (if (empty? ps-rest)
                    true ;; Case 1: No more pattern elements after _*, so it matches anything
                    ;; Case 2: Try matching remaining pattern at each possible position
                    (loop [k 0]
                      (cond
                        ;; We've gone beyond the end of ds, no match
                        (> k (count ds))
                        false

                        ;; Try matching rest of pattern against rest of data starting at position k
                        (match-seq ps-rest (drop k ds))
                        true

                        ;; Try next position
                        :else
                        (recur (inc k))))))

                ;; nested list/vector ⇒ recurse
                (and (sequential? (first ps))
                     (sequential? (first ds)))
                (and (match-sexpr (first ps) (first ds))
                     (recur (rest ps) (rest ds)))

                ;; literal equality
                :else
                (and (= (first ps) (first ds))
                     (recur (rest ps) (rest ds)))))]
      (match-seq pattern data))
    (= pattern '_?) true
    (= pattern '_*) true
    ;; atoms ⇒ direct equality
    :else
    (= pattern data)))

(defn find-match*
  [pattern-sexpr zloc]
  (loop [loc zloc]
    (when-not (z/end? loc)
      (let [form (try (z/sexpr loc)
                      (catch Exception _e
                        ::continue))]
        (if (= ::continue form)
          (recur (z/next loc))
          (if (match-sexpr pattern-sexpr form)
            loc
            (recur (z/next loc))))))))

(defn find-match [pattern-str code-str]
  (find-match* (z/sexpr (z/of-string pattern-str))
               (z/of-string code-str)))

(comment

  ;; Define a multimethod for calculating area
  (defmulti area :type)

  (defmethod area :circle
    [shape]
    (let [{:keys [radius]} shape]
      ;; Using exact Math/PI constant instead of hardcoded value
      (* Math/PI radius radius))))

#_(z/string (find-match "(defmethod area :circle [shape]
   (let [{:keys [radius]} shape]
;; Using exact Math/PI constant instead of hardcoded value
(* _* radius radius )
  ))" (slurp "src/clojure_mcp/sexp/match.clj")))

;; Example tests
#_(match-sexpr '(+ _* radius radius) '(+ 3 radius radius))
#_(match-sexpr '(defn calc [x y] _* (if (> x 10) x y))
               '(defn calc [x y] (println "calculating") (if (> x 10) x y)))
#_(match-sexpr '(* Math/PI radius radius) '(* Math/PI radius radius)) ;; Regular * operators match normally
