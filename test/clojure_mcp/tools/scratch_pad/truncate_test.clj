(ns clojure-mcp.tools.scratch-pad.truncate-test
  (:require [clojure-mcp.tools.scratch-pad.truncate :as truncate]
            [clojure.test :refer [deftest is testing]]))

(deftest truncate-depth-test
  (testing "Basic truncation at different depths"
    (let [data {:a {:b {:c {:d "deep"}}}}]
      (is (= {'... '...1_entries}
             (truncate/truncate-depth data 0)))
      (is (= {:a {'... '...1_entries}}
             (truncate/truncate-depth data 1)))
      (is (= {:a {:b {'... '...1_entries}}}
             (truncate/truncate-depth data 2)))
      (is (= {:a {:b {:c {'... '...1_entries}}}}
             (truncate/truncate-depth data 3)))))

  (testing "Leaf values always become ellipsis"
    (let [data {:string "test"
                :number 42
                :keyword :key
                :nested {:value "hidden"}}]
      (is (= {:string '...
              :number '...
              :keyword '...
              :nested {'... '...1_entries}}
             (truncate/truncate-depth data 1)))))

  (testing "Empty collections at max depth"
    (let [data {:empty-vec []
                :empty-map {}
                :empty-set #{}}]
      (is (= {:empty-vec '...
              :empty-map {'... '...0_entries}
              :empty-set #{'...}}
             (truncate/truncate-depth data 1)))))

  (testing "Size indicators for sequences at max depth"
    (is (= ['...10_elements]
           (truncate/truncate-depth (vec (range 10)) 0)))
    (is (= ['...5_elements]
           (truncate/truncate-depth (vec (range 5)) 0))))

  (testing "Size indicators for maps at max depth"
    (let [large-map (zipmap (range 10) (repeat "val"))]
      (is (= {'... '...10_entries}
             (truncate/truncate-depth large-map 0)))))

  (testing "Size indicators for sets at max depth"
    (is (= #{'...8_items}
           (truncate/truncate-depth #{:a :b :c :d :e :f :g :h} 0))))

  (testing "Preserves collection types"
    (let [data {:sorted (sorted-map :z 1 :a 2)
                :list '(1 2 3)
                :vec [1 2 3]}
          result (truncate/truncate-depth data 1)]
      (is (sorted? (:sorted result)))
      (is (list? (:list result)))
      (is (vector? (:vec result)))))

  (testing "Custom ellipsis"
    (is (= {:a {'<...> '...1_entries}}
           (truncate/truncate-depth {:a {:b {:c 1}}} 1 {:ellipsis '<...>}))))

  (testing "Disable size indicators"
    (is (= '...
           (truncate/truncate-depth (vec (range 5)) 0 {:show-size false})))))

(deftest pprint-truncated-test
  (testing "Pretty prints truncated data"
    (let [result (truncate/pprint-truncated {:a {:b {:c 1}}} 1)]
      (is (string? result))
      (is (re-find #"\.\.\." result)))))
