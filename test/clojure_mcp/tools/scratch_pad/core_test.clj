(ns clojure-mcp.tools.scratch-pad.core-test
  (:require [clojure-mcp.tools.scratch-pad.core :as core]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest test-execute-set-path
  (testing "Setting values at paths"
    (let [result (core/execute-set-path {} ["a" "b"] 42)]
      (is (= {"a" {"b" 42}} (:data result)))
      (is (= 42 (get-in (:result result) [:value]))))

    (testing "Setting with string indices for vectors"
      (let [result (core/execute-set-path {} ["tasks" "0"] "first")]
        (is (= {"tasks" ["first"]} (:data result)))
        (is (= "first" (get-in (:result result) [:value])))))

    (testing "Nested vector initialization"
      (let [result (core/execute-set-path {} ["data" "0" :items "0"] "nested")]
        (is (= {"data" [{:items ["nested"]}]} (:data result)))))))

(deftest test-execute-get-path
  (testing "Getting values from paths"
    (let [data {"a" {"b" 42}}
          result (core/execute-get-path data ["a" "b"])]
      (is (= 42 (get-in result [:result :value])))
      (is (true? (get-in result [:result :found]))))

    (testing "Getting with string indices"
      (let [data {"items" ["a" "b" "c"]}
            result (core/execute-get-path data ["items" "1"])]
        (is (= "b" (get-in result [:result :value])))))

    (testing "Getting non-existent path"
      (let [result (core/execute-get-path {} ["missing"])]
        (is (nil? (get-in result [:result :value])))
        (is (false? (get-in result [:result :found])))))))

(deftest test-execute-delete-path
  (testing "Deleting values at paths"
    (let [data {"a" {"b" 2 "c" 3}}
          result (core/execute-delete-path data ["a" "b"])]
      (is (= {"a" {"c" 3}} (:data result))))

    (testing "Deleting from vector with string index"
      (let [data {"items" ["a" "b" "c"]}
            result (core/execute-delete-path data ["items" "1"])]
        (is (= {"items" ["a" "c"]} (:data result)))))

    (testing "Deleting entire key"
      (let [data {"a" {"b" 2}}
            result (core/execute-delete-path data ["a"])]
        (is (= {} (:data result)))))))

(deftest test-execute-inspect
  (testing "Inspect entire data"
    (let [data {"a" 1}
          result (core/execute-inspect data 5 nil)
          tree-output (get-in result [:result :tree])]
      (is (string? tree-output))
      ;; Check that output contains the key "a" 
      (is (.contains tree-output "\"a\""))
      ;; Output might be truncated or formatted differently
      (is (or (.contains tree-output "{\"a\" 1}")
              (.contains tree-output "{\"a\" ...}")))))

  (testing "Inspect at path"
    (let [data {"a" {"b" {"c" 1}}}
          result (core/execute-inspect data 5 ["a"])
          tree-output (get-in result [:result :tree])]
      (is (.contains tree-output "{\"b\""))
      ;; The nested structure might be truncated
      (is (or (.contains tree-output "{\"c\" 1}")
              (.contains tree-output "{\"c\" ...}")))))

  (testing "Inspect non-existent path"
    (let [result (core/execute-inspect {} 5 ["missing"])]
      (is (= "No data found at path [\"missing\"]" (get-in result [:result :tree])))))

  (testing "Empty data inspect"
    (let [result (core/execute-inspect {} 5 nil)
          tree-output (get-in result [:result :tree])]
      ;; Account for possible newline
      (is (= "{}" (str/trim tree-output))))))

(deftest test-inspect-data
  (testing "Inspect data generation"
    (is (= "Empty scratch pad" (core/inspect-data {})))
    (testing "Pretty printing simple data"
      (let [data {"a" 1}
            result (core/inspect-data data)]
        (is (string? result))
        (is (.contains result "{\"a\" 1}"))))
    (testing "Pretty printing nested data"
      (let [nested {"a" {"b" {"c" 1}}}
            view (core/inspect-data nested)]
        (is (.contains view "{\"a\""))))))

(deftest test-smart-path-integration
  (testing "Smart path operations work correctly"
    ;; Test vector initialization with string "0"
    (let [result (core/execute-set-path {} ["items" "0"] "first")]
      (is (= {"items" ["first"]} (:data result))))

    ;; Test getting from vector with string index
    (let [data {"items" ["a" "b" "c"]}
          result (core/execute-get-path data ["items" "2"])]
      (is (= "c" (get-in result [:result :value]))))

    ;; Test error on invalid vector initialization
    (is (thrown? Exception
                 (core/execute-set-path {} ["items" "1"] "should-fail")))))
