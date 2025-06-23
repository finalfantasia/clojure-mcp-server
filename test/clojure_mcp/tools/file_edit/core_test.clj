(ns clojure-mcp.tools.file-edit.core-test
  (:require [clojure-mcp.tools.file-edit.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

;; Dynamic variables for test directories and files
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-file* nil)
(def ^:dynamic *test-nested-dir* nil)

;; Test fixture to create and cleanup test files/directories
(defn create-test-files-fixture [f]
  (let [tmp-dir (io/file (System/getProperty "java.io.tmpdir")
                         (str "file-edit-test-" (System/currentTimeMillis)))
        test-file (io/file tmp-dir "test-file.txt")
        test-nested-dir (io/file tmp-dir "nested")]

    ;; Create test directory and file
    (.mkdirs tmp-dir)
    (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")

    ;; Bind dynamic vars for the test
    (binding [*test-dir* tmp-dir
              *test-file* test-file
              *test-nested-dir* test-nested-dir]

      ;; Run the test
      (try
        (f)
        (finally
          ;; Cleanup
          (when (.exists test-file)
            (.delete test-file))
          (when (.exists test-nested-dir)
            (.delete test-nested-dir))
          (when (.exists tmp-dir)
            (.delete tmp-dir)))))))

;; Use the fixture for all tests
(use-fixtures :each create-test-files-fixture)

(deftest load-file-content-test
  (testing "Loading existing file content"
    (let [result (core/load-file-content (.getAbsolutePath *test-file*))]
      (is (not (:error result)) "Should not have error when file exists")
      (is (= "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n" (:content result)) "Content should match file")))

  (testing "Loading non-existent file content"
    (let [result (core/load-file-content (.getAbsolutePath (io/file *test-dir* "nonexistent.txt")))]
      (is (:error result) "Should have error when file doesn't exist")
      (is (string? (:message result)) "Should include error message"))))

(deftest count-occurrences-test
  (testing "Count occurrences of substring"
    (let [test-string "one two one three one four"]
      (is (= 3 (core/count-occurrences test-string "one")) "Should find 3 occurrences of 'one'")
      (is (= 1 (core/count-occurrences test-string "two")) "Should find 1 occurrence of 'two'")
      (is (= 0 (core/count-occurrences test-string "five")) "Should find 0 occurrences of 'five'")
      (is (= 0 (core/count-occurrences test-string "")) "Should handle empty search string"))))

(deftest validate-file-edit-test
  (testing "Validation with identical strings"
    (let [result (core/validate-file-edit
                  (.getAbsolutePath *test-file*)
                  "Line 3"
                  "Line 3"
                  "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")]
      (is (not (:valid result)) "Should be invalid when strings are identical")
      (is (string? (:message result)) "Should include error message")))

  (testing "Validation for empty old_string (attempt to create new file)"
    (let [result (core/validate-file-edit
                  (.getAbsolutePath (io/file *test-nested-dir* "new-file.txt"))
                  ""
                  "New content"
                  nil)]
      (is (not (:valid result)) "Should be invalid when using empty old_string")
      (is (str/includes? (:message result) "Empty old_string is not supported") "Should direct to file_write instead")))

  (testing "Validation for empty old_string with existing file"
    (let [result (core/validate-file-edit
                  (.getAbsolutePath *test-file*)
                  ""
                  "New content"
                  "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")]
      (is (not (:valid result)) "Should be invalid when using empty old_string")
      (is (str/includes? (:message result) "Empty old_string is not supported") "Should direct to file_write instead")))

  (testing "Validation for non-existent file"
    (let [result (core/validate-file-edit
                  (.getAbsolutePath (io/file *test-dir* "nonexistent.txt"))
                  "Line 3"
                  "Line 3 - EDITED"
                  nil)]
      (is (not (:valid result)) "Should be invalid when file doesn't exist")
      (is (string? (:message result)) "Should include error message")))

  (testing "Validation for non-matching string"
    (let [result (core/validate-file-edit
                  (.getAbsolutePath *test-file*)
                  "Not in file"
                  "Replacement"
                  "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")]
      (is (not (:valid result)) "Should be invalid when string not found")
      (is (string? (:message result)) "Should include error message")))

  (testing "Validation for non-unique match"
    (let [test-content "Line\nLine\nLine\nLine\n"
          duplicate-file (io/file *test-dir* "duplicate.txt")]
      (spit duplicate-file test-content)
      (try
        (let [result (core/validate-file-edit
                      (.getAbsolutePath duplicate-file)
                      "Line"
                      "Edited Line"
                      test-content)]
          (is (not (:valid result)) "Should be invalid for non-unique match")
          (is (string? (:message result)) "Should include error message")
          (is (str/includes? (:message result) "4 matches") "Should mention match count"))
        (finally
          (when (.exists duplicate-file)
            (.delete duplicate-file))))))

  (testing "Validation for valid edit"
    (let [result (core/validate-file-edit
                  (.getAbsolutePath *test-file*)
                  "Line 3"
                  "Line 3 - EDITED"
                  "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")]
      (is (:valid result) "Should be valid for unique match"))))

(deftest perform-file-edit-test
  (testing "Perform edit on existing file"
    (let [content "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n"
          result (core/perform-file-edit
                  "Line 3"
                  "Line 3 - EDITED"
                  content)]
      (is (= "Line 1\nLine 2\nLine 3 - EDITED\nLine 4\nLine 5\n" result) "Content should be edited correctly"))))

(deftest save-file-content-test
  (testing "Save to existing file"
    (let [new-content "Line 1\nLine 2\nModified\nLine 4\nLine 5\n"
          result (core/save-file-content (.getAbsolutePath *test-file*) new-content)]
      (is (:success result) "Save should succeed")
      (is (= new-content (slurp *test-file*)) "File content should be updated")))

  (testing "Create parent directories and save"
    (let [nested-file (io/file *test-nested-dir* "deep/path/new-file.txt")
          result (core/save-file-content (.getAbsolutePath nested-file) "New nested content")]
      (is (:success result) "Save should succeed")
      (is (.exists nested-file) "File should be created")
      (is (= "New nested content" (slurp nested-file)) "File content should match")

      ;; Clean up created directories
      (.delete nested-file)
      (.delete (.getParentFile nested-file))
      (.delete (.getParentFile (.getParentFile nested-file))))))