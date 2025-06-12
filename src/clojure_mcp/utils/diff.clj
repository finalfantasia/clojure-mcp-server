(ns clojure-mcp.utils.diff
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str])
  (:import (java.io File)))

(defn diff-has-tmp-file-headers?
  [lines]
  (and (>= (count lines) 2)
       (str/starts-with? (first lines) "---")
       (str/includes? (first lines) "clj-diff-")
       (str/starts-with? (nth lines 1) "+++")
       (str/includes? (nth lines 1) "clj-diff-")))

(defn truncate-diff-output
  [diff-output]
  (if (str/blank? diff-output)
    diff-output
    (let [lines (str/split-lines diff-output)]
      (if (diff-has-tmp-file-headers? lines)
        (str/join "\n" (drop 2 lines))
        diff-output))))

(defn generate-diff-via-shell
  "Generates a unified diff between two strings using the external diff command.
   Truncates the first two lines which contain temporary file paths.
   Requires two strings (file1-content, file2-content) and context-lines count."
  [file1-content file2-content context-lines]
  (let [temp-prefix "clj-diff-"
        temp-suffix ".tmp"
        file1 (File/createTempFile temp-prefix temp-suffix)
        file2 (File/createTempFile temp-prefix temp-suffix)]
    (try
      (spit file1 file1-content)
      (spit file2 file2-content)

      (let [file1-path (.getAbsolutePath file1)
            file2-path (.getAbsolutePath file2)
            ;; Use -U for unified diff format with specified context
            diff-result (shell/sh "diff" "-U" (str context-lines) file1-path file2-path)
            diff-output (if (= 0 (:exit diff-result))
                          ;; Files are identical or diff succeeded
                          (:out diff-result)
                          ;; Diff command indicated differences (exit code 1) or an error (other non-zero)
                          ;; Exit code 1 is normal for differences, so we still return the output.
                          ;; Handle other errors specifically if needed.
                          (if (= 1 (:exit diff-result))
                            (:out diff-result)
                            (throw (ex-info "Diff command failed" {:result diff-result}))))]
        ;; Truncate the first two lines (temporary file paths)
        (truncate-diff-output diff-output))
      (finally
        ;; Ensure temporary files are deleted
        (.delete file1)
        (.delete file2)))))
