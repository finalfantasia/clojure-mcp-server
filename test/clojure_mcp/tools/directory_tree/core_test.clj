(ns clojure-mcp.tools.directory-tree.core-test
  (:require [clojure-mcp.tools.directory-tree.core :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest directory-tree-test
  (testing "directory-tree generates proper tree structure"
    (let [temp-dir (io/file (System/getProperty "java.io.tmpdir") "directory-tree-test")]

      ;; Set up test directory structure
      (.mkdirs temp-dir)
      (let [dir1 (io/file temp-dir "dir1")
            dir2 (io/file temp-dir "dir2")
            file1 (io/file temp-dir "file1.txt")
            file2 (io/file dir1 "file2.txt")]

        (try
          (.mkdirs dir1)
          (.mkdirs dir2)
          (spit file1 "content1")
          (.mkdirs (.getParentFile file2))
          (spit file2 "content2")

          (testing "full tree"
            (let [result (sut/directory-tree (.getAbsolutePath temp-dir))]
              (is (string? result))
              (is (.contains result "dir1/"))
              (is (.contains result "dir2/"))
              (is (.contains result "file1.txt"))))

          (testing "with max-depth 0"
            (let [result (sut/directory-tree (.getAbsolutePath temp-dir) :max-depth 0)]
              (is (string? result))
              (is (.contains result "dir1/"))
              (is (.contains result "dir2/"))
              (is (.contains result "file1.txt"))
              (is (.contains result "..."))
              (is (not (.contains result "file2.txt")))))

          (finally
            ;; Clean up
            (io/delete-file file2 true)
            (io/delete-file file1 true)
            (io/delete-file dir1 true)
            (io/delete-file dir2 true)
            (io/delete-file temp-dir true))))))

  (testing "directory-tree with non-existent directory"
    (let [result (sut/directory-tree "/path/that/does/not/exist")]
      (is (map? result))
      (is (contains? result :error))
      (is (.contains (:error result) "not a valid directory")))))