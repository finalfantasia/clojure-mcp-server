(ns clojure-mcp.other-tools.move-file.core-test
  (:require [clojure-mcp.other-tools.move-file.core :as core]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]))

;; Dynamic variables for test directories and files
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *test-source* nil)
(def ^:dynamic *test-dest* nil)

;; Test fixture to create and cleanup test files/directories
(defn create-test-files-fixture [f]
  (let [tmp-dir (io/file (System/getProperty "java.io.tmpdir") "move-file-test")
        source-file (io/file tmp-dir "source.txt")
        dest-file (io/file tmp-dir "dest.txt")]

    ;; Create test directory
    (.mkdirs tmp-dir)

    ;; Create test source file
    (spit source-file "Test content for move operation")

    ;; Bind dynamic vars for the test
    (binding [*test-dir* tmp-dir
              *test-source* (.getAbsolutePath source-file)
              *test-dest* (.getAbsolutePath dest-file)]

      ;; Run the test
      (try
        (f)
        (finally
          ;; Cleanup
          (when (.exists dest-file)
            (.delete dest-file))
          (when (.exists source-file)
            (.delete source-file))
          (when (.exists tmp-dir)
            (.delete tmp-dir)))))))

;; Use the fixture for all tests
(use-fixtures :each create-test-files-fixture)

(deftest move-file-success-test
  (testing "Successfully moving a file"
    (let [result (core/move-file *test-source* *test-dest*)]
      (is (:success result) "Move operation should succeed")
      (is (= "file" (:type result)) "Should identify as file type")
      (is (not (.exists (io/file *test-source*))) "Source file should no longer exist")
      (is (.exists (io/file *test-dest*)) "Destination file should exist")
      (is (= "Test content for move operation"
             (slurp *test-dest*)) "Content should be preserved"))))

(deftest move-file-failure-tests
  (testing "Failure when source doesn't exist"
    (let [non-existent-source (str *test-dir* "/non-existent.txt")
          result (core/move-file non-existent-source *test-dest*)]
      (is (not (:success result)) "Move should fail")
      (is (:error result) "Error message should be provided")
      (is (str/includes? (:error result) "does not exist")
          "Error should mention non-existent source")))

  (testing "Failure when destination already exists"
    ;; Create the destination file
    (spit *test-dest* "Existing content")
    (let [result (core/move-file *test-source* *test-dest*)]
      (is (not (:success result)) "Move should fail")
      (is (:error result) "Error message should be provided")
      (is (str/includes? (:error result) "already exists")
          "Error should mention existing destination")
      (is (.exists (io/file *test-source*)) "Source file should still exist")
      (is (= "Test content for move operation"
             (slurp *test-source*)) "Source content should be preserved")
      (is (= "Existing content"
             (slurp *test-dest*)) "Destination content should be unchanged"))))