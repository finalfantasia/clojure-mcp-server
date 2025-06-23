(ns clojure-mcp.tools.scratch-pad.smart-path-test
  (:require [clojure-mcp.tools.scratch-pad.smart-path :as sp]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest test-parse-positive-int
  (testing "parse-positive-int helper"
    (is (= 0 (sp/parse-positive-int "0")))
    (is (= 123 (sp/parse-positive-int "123")))
    (is (nil? (sp/parse-positive-int "-1")))
    (is (nil? (sp/parse-positive-int "abc")))
    (is (nil? (sp/parse-positive-int nil)))
    (is (nil? (sp/parse-positive-int 123)))))

(deftest test-negative-number-string?
  (testing "negative-number-string? helper"
    (is (true? (sp/negative-number-string? "-1")))
    (is (true? (sp/negative-number-string? "-123")))
    (is (false? (sp/negative-number-string? "1")))
    (is (false? (sp/negative-number-string? "-abc")))
    (is (false? (sp/negative-number-string? nil)))))

(deftest test-smart-get-in
  (testing "smart-get-in with string indices"
    (is (= "b" (sp/smart-get-in {"items" ["a" "b" "c"]} ["items" "1"])))
    (is (= "b" (sp/smart-get-in {"items" ["a" "b" "c"]} ["items" 1])))
    (is (nil? (sp/smart-get-in {"items" ["a" "b"]} ["items" "999"])))
    (is (nil? (sp/smart-get-in {} ["missing" "0"]))))

  (testing "smart-get-in with maps"
    (is (= "value" (sp/smart-get-in {"map" {"key" "value"}} ["map" "key"])))
    (is (= "negative" (sp/smart-get-in {"map" {"-1" "negative"}} ["map" "-1"])))))

(deftest test-smart-assoc-in
  (testing "vector initialization at index 0"
    (is (= {"tasks" ["first"]}
           (sp/smart-assoc-in {} ["tasks" "0"] "first")))
    (is (= {"tasks" ["first"]}
           (sp/smart-assoc-in {} ["tasks" 0] "first"))))

  (testing "nested structure initialization"
    (is (= {"data" [{:items ["nested"]}]}
           (sp/smart-assoc-in {} ["data" "0" :items "0"] "nested"))))

  (testing "adding to existing vector"
    (is (= {"tasks" ["a" "b"]}
           (sp/smart-assoc-in {"tasks" ["a"]} ["tasks" "1"] "b")))
    (is (= {"tasks" ["a" "b" "c"]}
           (sp/smart-assoc-in {"tasks" ["a" "b"]} ["tasks" "2"] "c"))))

  (testing "map operations preserve string keys"
    (is (= {"config" {"-1" "negative"}}
           (sp/smart-assoc-in {"config" {}} ["config" "-1"] "negative")))
    (is (= {"config" {"123" "string-key"}}
           (sp/smart-assoc-in {"config" {}} ["config" "123"] "string-key")))))

(deftest test-smart-update-in
  (testing "update with function"
    (is (= {"count" 6}
           (sp/smart-update-in {"count" 5} ["count"] inc)))
    (is (= {"items" ["A" "b"]}
           (sp/smart-update-in {"items" ["a" "b"]} ["items" "0"] str/upper-case)))))

(deftest test-smart-dissoc-in
  (testing "dissoc from vector"
    (is (= {"items" ["a" "c"]}
           (sp/smart-dissoc-in {"items" ["a" "b" "c"]} ["items" "1"])))
    (is (= {"items" ["b" "c"]}
           (sp/smart-dissoc-in {"items" ["a" "b" "c"]} ["items" 0]))))

  (testing "dissoc from map"
    (is (= {"config" {}}
           (sp/smart-dissoc-in {"config" {"key" "value"}} ["config" "key"])))
    (is (= {"config" {}}
           (sp/smart-dissoc-in {"config" {"-1" "negative"}} ["config" "-1"])))))

(deftest test-error-cases
  (testing "cannot initialize vector at non-zero index"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Cannot initialize vector with non-zero index"
                          (sp/smart-assoc-in {} ["tasks" "1"] "error")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Cannot initialize vector with non-zero index"
                          (sp/smart-assoc-in {} ["tasks" 2] "error"))))

  (testing "vector index out of bounds"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Vector index out of bounds"
                          (sp/smart-assoc-in {"items" ["a"]} ["items" "5"] "error")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Vector index out of bounds"
                          (sp/smart-assoc-in {"items" ["a" "b"]} ["items" 10] "error"))))

  (testing "string keys not allowed for vectors"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"String keys not allowed for vectors"
                          (sp/smart-assoc-in {"items" ["a"]} ["items" "foo"] "error")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"String keys not allowed for vectors"
                          (sp/smart-assoc-in {"items" []} ["items" "bar"] "error"))))

  (testing "negative indices not allowed for vectors"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Negative indices not allowed for vectors"
                          (sp/smart-assoc-in {"items" ["a"]} ["items" "-1"] "error")))
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"Negative indices not allowed for vectors"
                          (sp/smart-dissoc-in {"items" ["a" "b"]} ["items" "-2"])))))

(deftest test-complex-scenarios
  (testing "mixed map and vector paths"
    (let [data {"users" [{:name "Alice" :tasks ["task1" "task2"]}
                         {:name "Bob" :tasks ["task3"]}]}]
      (is (= "task2"
             (sp/smart-get-in data ["users" "0" :tasks "1"])))
      (is (= {"users" [{:name "Alice" :tasks ["task1" "TASK2"]}
                       {:name "Bob" :tasks ["task3"]}]}
             (sp/smart-assoc-in data ["users" "0" :tasks "1"] "TASK2")))))

  (testing "deeply nested initialization"
    (is (= {"app" {:data [{:items [{:name "item1"}]}]}}
           (sp/smart-assoc-in {} ["app" :data "0" :items "0" :name] "item1"))))

  (testing "update operations with string indices"
    (let [data {"scores" [10 20 30]}]
      (is (= {"scores" [10 21 30]}
             (sp/smart-update-in data ["scores" "1"] inc)))
      (is (= {"scores" [10 40 30]}
             (sp/smart-update-in data ["scores" "1"] * 2))))))

(deftest test-edge-cases
  (testing "empty path operations"
    (is (= {:a 1} (sp/smart-get-in {:a 1} [])))
    (is (= "replaced" (sp/smart-assoc-in {:a 1} [] "replaced")))
    (is (= {:a 2} (sp/smart-update-in {:a 1} [] assoc :a 2)))
    (is (= {:a 1} (sp/smart-dissoc-in {:a 1} []))))

  (testing "operations on nil"
    (is (nil? (sp/smart-get-in nil ["anything"])))
    (is (= {"key" "value"} (sp/smart-assoc-in nil ["key"] "value"))))

  (testing "string number keys in maps are preserved"
    (let [data {"map" {"0" "zero" "1" "one"}}]
      (is (= "zero" (sp/smart-get-in data ["map" "0"])))
      (is (= {"map" {"0" "ZERO" "1" "one"}}
             (sp/smart-assoc-in data ["map" "0"] "ZERO"))))))
