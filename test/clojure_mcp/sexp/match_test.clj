(ns clojure-mcp.sexp.match-test
  (:require [clojure-mcp.sexp.match :as match]
            [clojure.test :refer [deftest is testing]]))

(deftest test-match-sexpr-basics
  (testing "Basic equality matches"
    (is (match/match-sexpr 'x 'x))
    (is (match/match-sexpr 42 42))
    (is (match/match-sexpr "hello" "hello"))
    (is (match/match-sexpr '(+ 1 2) '(+ 1 2)))
    (is (not (match/match-sexpr 'x 'y)))
    (is (not (match/match-sexpr 42 43)))
    (is (not (match/match-sexpr '(+ 1 2) '(+ 1 3))))))

(deftest test-match-single-wildcard
  (testing "Single element wildcard _?"
    (is (match/match-sexpr '_? 'anything))
    (is (match/match-sexpr '_? 42))
    (is (match/match-sexpr '_? '(nested list)))

    (is (match/match-sexpr '(+ _? 2) '(+ 1 2)))
    (is (match/match-sexpr '(+ _? 2) '(+ (+ 3 4) 2)))
    (is (match/match-sexpr '(+ 1 _?) '(+ 1 2)))
    (is (match/match-sexpr '(_? 1 2) '(+ 1 2)))

    (is (not (match/match-sexpr '(+ _?) '(+ 1 2)))) ; Too many elements
    (is (not (match/match-sexpr '(+ _? _?) '(+ 1)))) ; Not enough elements

    (is (match/match-sexpr '(let [_? _?] _?) '(let [x 10] (+ x 2))))))

(deftest test-match-multiple-wildcard
  (testing "Multiple element wildcard _*"
    (is (match/match-sexpr '_* 'anything))
    (is (match/match-sexpr '_* 42))
    (is (match/match-sexpr '_* '(nested list)))

    ;; _* at the end position (matches any remaining elements)
    (is (match/match-sexpr '(+ 1 _*) '(+ 1)))
    (is (match/match-sexpr '(+ 1 _*) '(+ 1 2)))
    (is (match/match-sexpr '(+ 1 _*) '(+ 1 2 3 4)))

    ;; _* at the beginning
    (is (match/match-sexpr '(_* 3 4) '(1 2 3 4)))
    (is (match/match-sexpr '(_* 3 4) '(3 4)))
    (is (not (match/match-sexpr '(_* 3 4) '(1 2 3))))

    ;; _* in the middle
    (is (match/match-sexpr '(+ _* 5) '(+ 5)))
    (is (match/match-sexpr '(+ _* 5) '(+ 1 5)))
    (is (match/match-sexpr '(+ _* 5) '(+ 1 2 3 5)))
    (is (not (match/match-sexpr '(+ _* 5) '(+ 1 2 3))))

    ;; Multiple _* wildcards
    (is (match/match-sexpr '(defn _* [_*] _*) '(defn foo [x y] (+ x y))))

    ;; Nested expressions with wildcards - two separate tests
    (is (match/match-sexpr '(_* defn _* [_*] _*) '(note important defn bar [x] x)))
    (is (match/match-sexpr '(_* defn _* [_*] _*) '(defn calc [a b] (* a b))))))

(deftest test-literal-star-plus
  (testing "Literal * and + operators"
    ;; Ensure * and + as regular symbols don't match as wildcards
    (is (match/match-sexpr '(* 2 3) '(* 2 3)))
    (is (not (match/match-sexpr '(* 2 3) '(* 2 4))))
    (is (not (match/match-sexpr '(+ * 3) '(+ 2 3))))
    (is (match/match-sexpr '(+ * 3) '(+ * 3)))))

(deftest test-nested-expressions
  (testing "Nested expression matching"
    (is (match/match-sexpr '(if _? _? _?)
                           '(if (> x 10) (println "large") (println "small"))))

    (is (match/match-sexpr '(let [_? _*] _*)
                           '(let [x 1] (println x) (+ x 2))))

    (is (match/match-sexpr '(defn _? [_*] (let [_? _?] _*))
                           '(defn foo [x y] (let [z (+ x y)] (println z) z))))

    (is (not (match/match-sexpr '(if _? _?)
                                '(if (> x 10) (println "large") (println "small")))))))

(deftest test-find-match
  (testing "Finding matches in code strings"
    (let [code "(defn foo [x] (+ x 2)) (defn bar [y] (* y 3))"
          match1 (match/find-match "(defn foo [x] _*)" code)
          match2 (match/find-match "(defn bar [_?] _*)" code)]
      (is (some? match1))
      (is (some? match2)))))

(deftest test-complex-cases
  (testing "Complex pattern matching cases"
    ;; Function with docstring
    (is (match/match-sexpr '(defn _? _? [_*] _*)
                           '(defn calc "Calculate something" [x y] (+ x y))))

    ;; Match defmethod with dispatch value
    (is (match/match-sexpr '(defmethod area :circle [_?] _*)
                           '(defmethod area :circle [{:keys [radius]}]
                              (* Math/PI radius radius))))

    ;; Mixed wildcards
    (is (match/match-sexpr '(defn _? [_? _? & _?] _*)
                           '(defn process [a b & more]
                              (apply + a b more))))

    ;; Vector patterns
    (is (match/match-sexpr '[_? _* :as _?]
                           '[x y z :as coords]))))

(deftest test-regression-cases
  (testing "Regression test cases that were previously bugs"
    ;; The key bug case - * in non-terminal position
    (is (match/match-sexpr '(+ _* radius radius) '(+ 3 radius radius)))

    ;; Multiple elements after wildcard
    (is (match/match-sexpr '(defn calc [x y] _* (+ x y))
                           '(defn calc [x y] (println "calculating") (+ x y))))

    ;; Wildcard at beginning
    (is (match/match-sexpr '(_* defn calc [x])
                           '(note this is important defn calc [x])))))
