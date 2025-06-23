(ns clojure-mcp.tools.glob-files.core-test
  (:require [clojure-mcp.tools.glob-files.core :as sut]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import (java.nio.file Paths)))

(deftest glob-files-test
  (testing "Basic glob patterns with existing directory"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "**/*.clj" :max-results 5)]
      (is (not (:error result)) "Should not return an error")
      (is (vector? (:filenames result)) "Should return filenames as a vector")
      (is (<= (count (:filenames result)) 5) "Should limit results to max-results")
      (is (or (not (:truncated result))
              (<= (count (:filenames result)) (:numFiles result)))
          "numFiles should be >= filenames count when truncated")
      (is (boolean? (:truncated result)) "truncated should be a boolean")
      (is (number? (:durationMs result)) "durationMs should be a number")))

  (testing "Filtering with specific pattern"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "**/glob_files/**/*.clj")]
      (is (not (:error result)) "Should not return an error")
      (is (vector? (:filenames result)) "Should return filenames as a vector")
      (is (every? #(.contains % "glob_files") (:filenames result))
          "All files should contain glob_files in the path")))

  (testing "Handling non-existent directory"
    (let [result (sut/glob-files "/nonexistent/directory" "*.clj")]
      (is (:error result) "Should return an error")
      (is (string? (:error result)) "Error should be a string")))

  (testing "Handling invalid glob pattern"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "[invalid-glob-pattern")]
      (is (not (:error result)) "May not return an error for invalid pattern")
      (is (= 0 (:numFiles result)) "Should find 0 files for invalid pattern")))

  (testing "Result truncation with small max-results"
    (let [current-dir (System/getProperty "user.dir")
          result (sut/glob-files current-dir "**/*.clj" :max-results 1)]
      (is (not (:error result)) "Should not return an error")
      (is (= 1 (count (:filenames result))) "Should return exactly 1 result")
      (is (:truncated result) "Should indicate results were truncated")))

  (testing "Finding files in root directory with **/*.ext pattern"
    (let [current-dir (System/getProperty "user.dir")
          ;; Get files using both patterns for comparison
          standard-result (sut/glob-files current-dir "**/*.md")
          #_#_root-only-result (sut/glob-files current-dir "*.md")
          ;; Count root-level .md files using Java file operations
          root-file-count (count (filter #(and (.isFile %)
                                               (.endsWith (.getName %) ".md"))
                                         (.listFiles (io/file current-dir))))]
      ;; The **/*.md pattern should also find files in the root directory
      (is (>= (count (:filenames standard-result)) root-file-count)
          "**/*.md pattern should find all root-level files")
      ;; Compare with direct root pattern to ensure we find the same files
      #_(is (= (count (:filenames root-only-result)) root-file-count)
            "*.md pattern should find all root-level files")
      ;; Check if root files exist in the results
      (let [path-obj (Paths/get current-dir (into-array String []))
            root-files-in-results (filter
                                   (fn [file-path]
                                     (let [file-obj (Paths/get file-path (into-array String []))
                                           rel-path (.relativize path-obj file-obj)]
                                       (and (= (.getNameCount rel-path) 1)
                                            (.endsWith (str rel-path) ".md"))))
                                   (:filenames standard-result))]
        (is (= (count root-files-in-results) root-file-count)
            "**/*.md pattern should find all root-level md files")))))
