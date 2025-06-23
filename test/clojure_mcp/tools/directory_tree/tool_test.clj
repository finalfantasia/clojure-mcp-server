(ns clojure-mcp.tools.directory-tree.tool-test
  (:require [clojure-mcp.config :as config]
            [clojure-mcp.tool-system :as tool-system]
            [clojure-mcp.tools.directory-tree.core :as directory-tree-core]
            [clojure-mcp.tools.directory-tree.tool :as sut]
            [clojure-mcp.utils.valid-paths :as valid-paths]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(deftest tool-name-test
  (testing "tool-name returns the correct name"
    (is (= "LS"
           (tool-system/tool-name {:tool-type :directory-tree})))))

(deftest tool-description-test
  (testing "tool-description returns a non-empty description"
    (let [description (tool-system/tool-description {:tool-type :directory-tree})]
      (is (string? description))
      (is (not (str/blank? description))))))

(deftest tool-schema-test
  (testing "tool-schema returns a valid schema with required path parameter"
    (let [schema (tool-system/tool-schema {:tool-type :directory-tree})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :path))
      (is (contains? (:properties schema) :max_depth))
      (is (= [:path] (:required schema))))))

(deftest validate-inputs-test
  (testing "validate-inputs properly validates and transforms inputs"
    (let [nrepl-client-atom (atom {})]
      (config/set-config! nrepl-client-atom :nrepl-user-dir "/base/dir")
      (config/set-config! nrepl-client-atom :allowed-directories ["/base/dir"])
      (let [tool-config {:tool-type :directory-tree
                         :nrepl-client-atom nrepl-client-atom}]

        (with-redefs [valid-paths/validate-path-with-client
                      (fn [path _]
                        (str "/validated" path))]

          (testing "with only path"
            (let [result (tool-system/validate-inputs tool-config {:path "/test/path"})]
              (is (= {:path "/validated/test/path"} result))))

          (testing "with path and max_depth"
            (let [result (tool-system/validate-inputs tool-config {:path "/test/path" :max_depth 3})]
              (is (= {:path "/validated/test/path"
                      :max_depth 3} result))))

          (testing "missing required path parameter"
            (is (thrown-with-msg? ExceptionInfo
                                  #"Missing required parameter: path"
                                  (tool-system/validate-inputs tool-config {})))))))))

(deftest execute-tool-test
  (testing "execute-tool calls core function with correct parameters"
    (with-redefs [directory-tree-core/directory-tree
                  (fn [path & {:keys [max-depth]}]
                    {:called-with {:path path
                                   :max-depth max-depth}})]

      (testing "with only path"
        (let [result (tool-system/execute-tool
                      {:tool-type :directory-tree}
                      {:path "/test/path"})]
          (is (= {:called-with {:path "/test/path"
                                :max-depth nil}} result))))

      (testing "with path and max_depth"
        (let [result (tool-system/execute-tool
                      {:tool-type :directory-tree}
                      {:path "/test/path" :max_depth 3})]
          (is (= {:called-with {:path "/test/path"
                                :max-depth 3}} result)))))))

(deftest format-results-test
  (testing "format-results correctly formats successful results"
    (let [result "Directory tree output"
          formatted (tool-system/format-results {:tool-type :directory-tree} result)]
      (is (= {:result ["Directory tree output"]
              :error false} formatted))))

  (testing "format-results correctly formats error results"
    (let [result {:error "Some error occurred"}
          formatted (tool-system/format-results {:tool-type :directory-tree} result)]
      (is (= {:result ["Some error occurred"]
              :error true} formatted)))))

(deftest registration-map-test
  (testing "directory-tree-tool returns a valid registration map"
    (let [nrepl-client-atom (atom {})
          reg-map (sut/directory-tree-tool nrepl-client-atom)]
      (is (= "LS" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (map? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))
