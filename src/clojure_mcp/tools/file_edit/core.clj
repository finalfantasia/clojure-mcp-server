(ns clojure-mcp.tools.file-edit.core
  "Core implementation for the file-edit tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import (java.util.regex Pattern)))

(defn load-file-content
  "Load file content from a path.

   Parameters:
   - path: Path to the file to load

   Returns a map with:
   - :content - The file contents as a string
   - :error - Boolean indicating if an error occurred
   - :message - Error message (only when :error is true)"
  [path]
  (try
    (let [file (io/file path)]
      (if (.exists file)
        {:content (slurp file)
         :error false}
        {:error true
         :message (str "File not found: " path)}))
    (catch Exception e
      {:error true
       :message (str "Error reading file: " (.getMessage e))})))

(defn count-occurrences
  "Count occurrences of a substring within a string."
  [string substring]
  (if (empty? substring)
    0
    (count (re-seq (re-pattern (Pattern/quote substring)) string))))

(defn suggest-similar-file
  "Try to find a similar file with a different extension.
   Return nil if no similar file found."
  [file-path]
  (let [f (io/file file-path)
        dir (.getParent f)
        name-without-ext (second (re-find #"(.+?)(?:\.[^.]*$|$)" (.getName f)))
        dir-file (io/file dir)
        similar-files (when (and dir-file (.exists dir-file) (.isDirectory dir-file))
                        (->> (.listFiles dir-file)
                             (filter #(.isFile %))
                             (filter #(str/starts-with? (.getName %) name-without-ext))
                             (filter #(not= (.getAbsolutePath %) file-path))))]
    (when (seq similar-files)
      (.getAbsolutePath (first similar-files)))))

(defn validate-file-edit
  "Validate a file edit operation.

   Parameters:
   - file-path: The path to the file
   - old-string: The string to replace
   - new-string: The replacement string
   - file-content: The current file content (or nil if file doesn't exist)

   Returns a map with:
   - :valid - Boolean indicating if the edit is valid
   - :message - Error message (only present when :valid is false)"
  [file-path old-string new-string file-content]
  (cond
    ;; Case 1: No changes (old-string and new-string are identical)
    (= old-string new-string)
    {:valid false
     :message "No changes to make: old_string and new_string are exactly the same."}

    ;; Case 2: Empty old-string (was previously used for new file creation)
    (empty? old-string)
    {:valid false
     :message "Empty old_string is not supported. To create a new file, use file_write instead."}

    ;; Case 3: File doesn't exist but trying to edit it
    (nil? file-content)
    (let [similar-file (suggest-similar-file file-path)
          message (str "File does not exist."
                       (when similar-file (str " Did you mean " similar-file "?")))]
      {:valid false
       :message message})

    ;; Case 4: No matches of old-string in file
    (not (str/includes? file-content old-string))
    {:valid false
     :message "String to replace not found in file."}

    ;; Case 5: Multiple matches of old-string (not unique)
    (> (count-occurrences file-content old-string) 1)
    (let [matches (count-occurrences file-content old-string)]
      {:valid false
       :message (str "Found " matches " matches of the string to replace. "
                     "For safety, this tool only supports replacing exactly one occurrence at a time. "
                     "Add more lines of context to your edit and try again.")})

    ;; Case 6: Valid edit
    :else
    {:valid true}))

(defn perform-file-edit
  "Perform the actual file edit operation.

   Parameters:
   - old-string: The string to replace
   - new-string: The replacement string
   - file-content: The current file content

   Returns the new file content."
  [old-string new-string file-content]
  ;; Edit existing file
  (str/replace-first file-content old-string new-string))

(defn save-file-content
  "Save content to a file, creating parent directories if needed.

   Parameters:
   - file-path: Path to the file to save
   - content: Content to save

   Returns a map with:
   - :success - Boolean indicating if the save was successful
   - :message - Error message (only when :success is false)"
  [file-path content]
  (try
    (let [file (io/file file-path)
          parent (.getParentFile file)]
      ;; Create parent directories if they don't exist
      (when (and parent (not (.exists parent)))
        (.mkdirs parent))
      (spit file content)
      {:success true})
    (catch Exception e
      {:success false
       :message (str "Failed to save file: " (.getMessage e))})))

(comment
  ;; === Examples of using the file-edit core functionality directly ===

  ;; Test file paths
  (def temp-dir (System/getProperty "java.io.tmpdir"))
  (def test-file (str temp-dir "/file-edit-test.txt"))

  ;; Create a test file
  (spit test-file "Line 1\nLine 2\nLine 3\nLine 4\nLine 5\n")

  ;; Test validation
  (validate-file-edit test-file "Line 3" "Line 3 - EDITED" (slurp test-file))
  (validate-file-edit test-file "Line" "EDITED Line" (slurp test-file))
  (validate-file-edit (str temp-dir "/nonexistent.txt") "Line" "EDITED Line" nil)
  ;; Test attempt to use empty old_string (should fail)
  (validate-file-edit test-file "" "New content" (slurp test-file))

  ;; Test editing
  (perform-file-edit "Line 3" "Line 3 - EDITED" (slurp test-file))

  ;; Clean up
  (.delete (io/file test-file)))
