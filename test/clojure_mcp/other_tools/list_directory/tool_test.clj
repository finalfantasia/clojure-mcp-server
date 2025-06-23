(ns clojure-mcp.other-tools.list-directory.tool-test
  (:require
   [clojure-mcp.config :as config]
   [clojure-mcp.other-tools.list-directory.core :as list-directory-core]
   [clojure-mcp.other-tools.list-directory.tool :as sut]
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.utils.valid-paths :as valid-paths]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]])
  (:import (clojure.lang ExceptionInfo)))

(deftest tool-name-test
  (testing "tool-name returns the correct name"
    (is (= "fs_list_directory"
           (tool-system/tool-name {:tool-type :list-directory})))))

(deftest tool-description-test
  (testing "tool-description returns a non-empty description"
    (let [description (tool-system/tool-description {:tool-type :list-directory})]
      (is (string? description))
      (is (not (str/blank? description))))))

(deftest tool-schema-test
  (testing "tool-schema returns a valid schema with required path parameter"
    (let [schema (tool-system/tool-schema {:tool-type :list-directory})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :path))
      (is (= [:path] (:required schema))))))

(deftest validate-inputs-test
  (testing "validate-inputs properly validates and transforms inputs"
    (let [nrepl-client-atom (atom {})]
      (config/set-config! nrepl-client-atom :nrepl-user-dir "/base/dir")
      (config/set-config! nrepl-client-atom :allowed-directories ["/base/dir"])
      (let [tool-config {:tool-type :list-directory
                         :nrepl-client-atom nrepl-client-atom}]

        (with-redefs [valid-paths/validate-path-with-client
                      (fn [path _]
                        (str "/validated" path))]

          (testing "with valid path"
            (let [result (tool-system/validate-inputs tool-config {:path "/test/path"})]
              (is (= {:path "/validated/test/path"} result))))

          (testing "missing required path parameter"
            (is (thrown-with-msg? ExceptionInfo
                                  #"Missing required parameter: path"
                                  (tool-system/validate-inputs tool-config {})))))))))

(deftest execute-tool-test
  (testing "execute-tool calls core function with correct parameters"
    (with-redefs [list-directory-core/list-directory
                  (fn [path]
                    {:called-with {:path path}})]

      (testing "with path parameter"
        (let [result (tool-system/execute-tool
                      {:tool-type :list-directory}
                      {:path "/test/path"})]
          (is (= {:called-with {:path "/test/path"}} result)))))))

(deftest format-results-test
  (testing "format-results correctly formats successful results"
    (let [result {:files ["file1.txt" "file2.clj"]
                  :directories ["dir1" "dir2"]
                  :full-path "/test/path"}
          formatted (tool-system/format-results {:tool-type :list-directory} result)]
      ;; Don't compare exact formatting string, just check content
      (is (false? (:error formatted)))
      (is (vector? (:result formatted)))
      (is (= 1 (count (:result formatted))))
      (is (string? (first (:result formatted))))
      (is (.contains (first (:result formatted)) "Directory: /test/path"))
      (is (.contains (first (:result formatted)) "[DIR] dir1"))
      (is (.contains (first (:result formatted)) "[DIR] dir2"))
      (is (.contains (first (:result formatted)) "[FILE] file1.txt"))
      (is (.contains (first (:result formatted)) "[FILE] file2.clj")))))

(deftest format-results-error-test
  (testing "format-results correctly formats error results"
    (let [result {:error "Some error occurred"}
          formatted (tool-system/format-results {:tool-type :list-directory} result)]
      (is (= {:result ["Some error occurred"]
              :error true} formatted)))))

(deftest format-directory-listing-test
  (testing "format-directory-listing correctly formats directory listing"
    (let [result {:files ["file1.txt" "file2.clj"]
                  :directories ["dir1" "dir2"]
                  :full-path "/test/path"}
          formatted (sut/format-directory-listing result)]
      (is (string? formatted))
      (is (.contains formatted "Directory: /test/path"))
      (is (.contains formatted "[DIR] dir1"))
      (is (.contains formatted "[DIR] dir2"))
      (is (.contains formatted "[FILE] file1.txt"))
      (is (.contains formatted "[FILE] file2.clj"))))

  (testing "format-directory-listing correctly formats empty directory"
    (let [result {:files []
                  :directories []
                  :full-path "/empty/dir"}
          formatted (sut/format-directory-listing result)]
      (is (string? formatted))
      (is (.contains formatted "Directory: /empty/dir"))
      (is (.contains formatted "Directory is empty"))))

  (testing "format-directory-listing correctly formats error"
    (let [result {:error "Directory not found"}
          formatted (sut/format-directory-listing result)]
      (is (string? formatted))
      (is (= "Directory not found" formatted)))))

(deftest registration-map-test
  (testing "list-directory-tool returns a valid registration map"
    (let [nrepl-client-atom (atom {})
          reg-map (sut/list-directory-tool nrepl-client-atom)]
      (is (= "fs_list_directory" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (map? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))