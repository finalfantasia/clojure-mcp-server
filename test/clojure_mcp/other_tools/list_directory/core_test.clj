(ns clojure-mcp.other-tools.list-directory.core-test
  (:require
   [clojure-mcp.other-tools.list-directory.core :as sut]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]))

(deftest list-directory-test
  (testing "list-directory lists files and directories correctly"
    (let [temp-dir (io/file (System/getProperty "java.io.tmpdir") "list-directory-test")]

      ;; Set up test directory structure
      (.mkdirs temp-dir)
      (let [dir1 (io/file temp-dir "dir1")
            dir2 (io/file temp-dir "dir2")
            file1 (io/file temp-dir "file1.txt")
            file2 (io/file temp-dir "file2.clj")]

        (try
          (.mkdirs dir1)
          (.mkdirs dir2)
          (spit file1 "content1")
          (spit file2 "content2")

          (testing "valid directory"
            (let [result (sut/list-directory (.getAbsolutePath temp-dir))]
              (is (map? result))
              (is (not (contains? result :error)))
              (is (contains? result :files))
              (is (contains? result :directories))
              (is (contains? result :full-path))
              (is (= (.getAbsolutePath temp-dir) (:full-path result)))
              (is (= 2 (count (:files result))))
              (is (= 2 (count (:directories result))))
              (is (some #(= "file1.txt" %) (:files result)))
              (is (some #(= "file2.clj" %) (:files result)))
              (is (some #(= "dir1" %) (:directories result)))
              (is (some #(= "dir2" %) (:directories result)))))

          (finally
            ;; Clean up
            (io/delete-file file1 true)
            (io/delete-file file2 true)
            (io/delete-file dir1 true)
            (io/delete-file dir2 true)
            (io/delete-file temp-dir true))))))

  (testing "list-directory with non-existent path"
    (let [result (sut/list-directory "/path/that/does/not/exist")]
      (is (map? result))
      (is (contains? result :error))
      (is (.contains (:error result) "does not exist"))))

  (testing "list-directory with file path instead of directory"
    (let [temp-file (io/file (System/getProperty "java.io.tmpdir") "test-file.txt")]
      (try
        (spit temp-file "test content")
        (let [result (sut/list-directory (.getAbsolutePath temp-file))]
          (is (map? result))
          (is (contains? result :error))
          (is (.contains (:error result) "is not a directory")))
        (finally
          (io/delete-file temp-file true))))))
