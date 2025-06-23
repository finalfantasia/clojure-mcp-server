(ns clojure-mcp.other-tools.create-directory.core-test
  (:require
   [clojure-mcp.other-tools.create-directory.core :as core]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]))

;; Dynamic variables for test directories and files
(def ^:dynamic *test-dir* nil)
(def ^:dynamic *nested-dir* nil)
(def ^:dynamic *conflict-file* nil)

;; Test fixture to set up a test environment
(defn create-test-environment-fixture [f]
  (let [tmp-dir (io/file (System/getProperty "java.io.tmpdir")
                         (str "create-dir-test-" (System/currentTimeMillis)))
        nested-dir (io/file tmp-dir "nested/path")
        conflict-file (io/file tmp-dir "file-not-dir")]

    ;; Create root test directory
    (.mkdirs tmp-dir)

    ;; Create a file that will conflict with directory path testing
    (spit conflict-file "test content")

    ;; Bind dynamic vars for the test
    (binding [*test-dir* tmp-dir
              *nested-dir* nested-dir
              *conflict-file* conflict-file]

      ;; Run the test
      (try
        (f)
        (finally
          ;; Cleanup
          (when (.exists conflict-file)
            (.delete conflict-file))
          (when (.exists nested-dir)
            (.delete nested-dir))
          (when (.exists (io/file tmp-dir "nested"))
            (.delete (io/file tmp-dir "nested")))
          (when (.exists tmp-dir)
            (.delete tmp-dir)))))))

;; Use the fixture for all tests
(use-fixtures :each create-test-environment-fixture)

(deftest create-new-directory-test
  (testing "Creating a new directory"
    (let [dir-path (.getAbsolutePath *nested-dir*)
          result (core/create-directory dir-path)]
      (is (:success result) "Directory creation should succeed")
      (is (:created result) "Should indicate directory was created")
      (is (not (:exists result)) "Should indicate directory did not previously exist")
      (is (.exists *nested-dir*) "Directory should now exist on disk")
      (is (.isDirectory *nested-dir*) "Should be a directory, not a file"))))

(deftest existing-directory-test
  (testing "Operation succeeds silently when directory already exists"
    ;; First, create the directory
    (.mkdirs *nested-dir*)
    (is (.exists *nested-dir*) "Setup check: directory should exist")

    ;; Then test calling create-directory on the existing directory
    (let [dir-path (.getAbsolutePath *nested-dir*)
          result (core/create-directory dir-path)]
      (is (:success result) "Operation should succeed")
      (is (not (:created result)) "Should indicate directory was not newly created")
      (is (:exists result) "Should indicate directory already existed")
      (is (.exists *nested-dir*) "Directory should still exist on disk"))))

(deftest file-conflict-test
  (testing "Operation fails when path exists as a file"
    (let [file-path (.getAbsolutePath *conflict-file*)
          result (core/create-directory file-path)]
      (is (not (:success result)) "Operation should fail")
      (is (:error result) "Should have an error message")
      (is (str/includes? (:error result) "is a file")
          "Error should mention path exists as a file"))))
