(ns clojure-mcp.tools.grep.tool-test
  (:require [clojure-mcp.config :as config]
            [clojure-mcp.tool-system :as tool-system]
            [clojure-mcp.tools.grep.core :as grep-core]
            [clojure-mcp.tools.grep.tool :as sut]
            [clojure-mcp.utils.valid-paths :as valid-paths]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(deftest tool-name-test
  (testing "tool-name returns the correct name"
    (is (= "grep"
           (tool-system/tool-name {:tool-type :grep})))))

(deftest tool-description-test
  (testing "tool-description returns a non-empty description"
    (let [description (tool-system/tool-description {:tool-type :grep})]
      (is (string? description))
      (is (not (str/blank? description))))))

(deftest tool-schema-test
  (testing "tool-schema returns a valid schema with required parameters"
    (let [schema (tool-system/tool-schema {:tool-type :grep})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :path))
      (is (contains? (:properties schema) :pattern))
      (is (contains? (:properties schema) :include))
      (is (contains? (:properties schema) :max_results))
      (is (= [:pattern] (:required schema))))))

(deftest validate-inputs-test
  (testing "validate-inputs properly validates and transforms inputs"
    (let [nrepl-client-atom (atom {})]
      (config/set-config! nrepl-client-atom :nrepl-user-dir "/base/dir")
      (config/set-config! nrepl-client-atom :allowed-directories ["/base/dir"])
      (let [tool-config {:tool-type :grep
                         :nrepl-client-atom nrepl-client-atom}]

        (with-redefs [valid-paths/validate-path-with-client
                      (fn [path _]
                        (str "/validated" path))]

          (testing "with only required parameters"
            (let [result (tool-system/validate-inputs
                          tool-config
                          {:path "/test/path" :pattern "test.*pattern"})]
              (is (= {:path "/validated/test/path"
                      :pattern "test.*pattern"} result))))

          (testing "with all parameters"
            (let [result (tool-system/validate-inputs
                          tool-config
                          {:path "/test/path"
                           :pattern "test.*pattern"
                           :include "*.clj"
                           :max_results 500})]
              (is (= {:path "/validated/test/path"
                      :pattern "test.*pattern"
                      :include "*.clj"
                      :max-results 500} result))))

          (testing "missing required pattern parameter"
            (is (thrown-with-msg? ExceptionInfo
                                  #"Missing required parameter: pattern"
                                  (tool-system/validate-inputs tool-config {:path "/test/path"})))))))))

(deftest execute-tool-test
  (testing "execute-tool calls core function with correct parameters"
    (with-redefs [grep-core/grep-files
                  (fn [path pattern & {:keys [include max-results]}]
                    {:called-with {:path path
                                   :pattern pattern
                                   :include include
                                   :max-results max-results}
                     :truncated (> 58 max-results)})]

      (testing "with only required parameters"
        (let [result (tool-system/execute-tool
                      {:tool-type :grep}
                      {:path "/test/path" :pattern "test.*pattern"})]
          (is (= {:called-with {:path "/test/path"
                                :pattern "test.*pattern"
                                :include nil
                                :max-results 1000}
                  :truncated false} result))))

      (testing "with all parameters"
        (let [result (tool-system/execute-tool
                      {:tool-type :grep}
                      {:path "/test/path"
                       :pattern "test.*pattern"
                       :include "*.clj"
                       :max-results 5})]
          (is (= {:called-with {:path "/test/path"
                                :pattern "test.*pattern"
                                :include "*.clj"
                                :max-results 5}
                  :truncated true} result)))))))

(deftest format-results-test
  (testing "format-results correctly formats successful results with files"
    (let [result {:filenames ["file1.clj" "file2.clj"]
                  :numFiles 2
                  :truncated false}
          formatted (tool-system/format-results {:tool-type :grep} result)]
      (is (= {:result ["Found 2 files\nfile1.clj\nfile2.clj"]
              :error false} formatted))))

  (testing "format-results correctly formats successful results with no files"
    (let [result {:filenames []
                  :numFiles 0
                  :truncated false}
          formatted (tool-system/format-results {:tool-type :grep} result)]
      (is (= {:result ["No files found"]
              :error false} formatted))))

  (testing "format-results correctly formats results with nil filenames"
    (let [result {:filenames nil
                  :durationMs 42}
          formatted (tool-system/format-results {:tool-type :grep} result)]
      (is (= {:result ["No files found"]
              :error false} formatted))))

  (testing "format-results correctly formats truncated results"
    (let [files (vec (map #(str "file" %) (range 1 6)))
          result {:filenames files
                  :numFiles 58
                  :truncated true}
          formatted (tool-system/format-results {:tool-type :grep} result)
          result-str (first (:result formatted))]
      (is (false? (:error formatted)))
      (is (str/starts-with? result-str "Found 58 files"))
      (is (str/includes? result-str "truncated"))))

  (testing "format-results correctly formats error results"
    (let [result {:error "Some error occurred"}
          formatted (tool-system/format-results {:tool-type :grep} result)]
      (is (= {:result ["Some error occurred"]
              :error true} formatted)))))

(deftest registration-map-test
  (testing "grep-tool returns a valid registration map"
    (let [nrepl-client-atom (atom {})
          reg-map (sut/grep-tool nrepl-client-atom)]
      (is (= "grep" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (map? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))
