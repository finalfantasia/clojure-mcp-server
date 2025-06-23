(ns clojure-mcp.tools.directory-tree.core
  "Core implementation for the directory-tree tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn ignored-file?
  "Checks if a file should be completely ignored (temp files, backups, etc.)."
  [file]
  (let [name (.getName file)]
    (or (.endsWith name "~") ; Emacs backup
        (.startsWith name "#") ; Emacs auto-save
        (.contains name ".#") ; Emacs lock files
        (= name ".DS_Store")
        (.endsWith name ".log"))))

(defn no-recurse-directory?
  "Checks if a directory should be shown but not recursed into (build/temp directories)."
  [file]
  (and (.isDirectory file)
       (let [name (.getName file)]
         (or (.startsWith name ".") ; Hidden directories like .git
             (= name "node_modules")
             (= name "target")
             (= name "out")
             (= name "build")
             (= name "dist")
             (= name ".cache")
             (= name ".tmp")
             (= name "temp")
             (= name "tmp")
             (= name "logs")))))

(defn format-entry
  "Formats a single file or directory entry with proper indentation."
  [entry indent is-directory?]
  (let [name (.getName entry)
        suffix (if is-directory? "/" "")]
    (str indent "- " name suffix "\n")))

(defn add-truncation-message
  "Adds a truncation message if there are remaining entries."
  [result indent remaining-count]
  (when (> remaining-count 0)
    (.append result (str indent "... truncated output (" remaining-count " more entries)\n"))))

(defn process-entries
  "Processes a list of entries until the limit is reached."
  [entries result indent entry-count limit]
  (loop [remaining entries
         processed 0]
    (if (or (empty? remaining) (>= @entry-count limit))
      (count remaining) ; Return count of remaining entries
      (let [entry (first remaining)
            is-dir? (.isDirectory entry)]
        (swap! entry-count inc)
        (.append result (format-entry entry indent is-dir?))
        (recur (rest remaining) (inc processed))))))

(defn directory-tree
  "Creates a recursive tree view of directory structure using simple indentation.
   
   Parameters:
   - path: The validated and normalized path to the directory
   - depth: Current depth level (used for indentation)
   - max-depth: Maximum depth to traverse (nil for unlimited)
   - limit: Maximum number of entries to show (default 100)
   - entry-count: Current count of entries shown (used internally)
   
   Returns a string representation of the directory tree with proper indentation.
   Shows build/temp directories but doesn't recurse into them.
   Adds truncation indicators when limits are reached."
  [path & {:keys [depth max-depth limit entry-count]
           :or {depth 0 max-depth nil limit 100 entry-count (atom 0)}}]
  (let [dir (io/file path)]
    (if (and (.exists dir) (.isDirectory dir))
      (let [indent (apply str (repeat depth "  "))
            contents (.listFiles dir)
            dirs (->> contents
                      (filter #(.isDirectory %))
                      (remove ignored-file?)
                      (sort-by #(.getName %)))
            files (->> contents
                       (filter #(.isFile %))
                       (remove ignored-file?)
                       (sort-by #(.getName %)))
            result (StringBuilder.)
            continue? (or (nil? max-depth) (< depth max-depth))]

        ;; Root entry
        (when (zero? depth)
          (.append result (str (.getAbsolutePath dir) "\n")))

        ;; Process directories with special handling for no-recurse directories
        (let [remaining-dirs
              (loop [remaining dirs]
                (if (or (empty? remaining) (>= @entry-count limit))
                  (count remaining)
                  (let [d (first remaining)]
                    (swap! entry-count inc)
                    (.append result (format-entry d indent true))

                    (cond
                      ;; If it's a no-recurse directory, show note and don't recurse
                      (no-recurse-directory? d)
                      (.append result (str indent "    (not expanded)\n"))

                      ;; If max-depth is set and we're at the limit
                      (and max-depth (= depth max-depth))
                      (.append result (str indent "    ...\n"))

                      ;; Otherwise process subdirectory if we should continue
                      continue?
                      (let [subtree (directory-tree (.getAbsolutePath d)
                                                    :depth (inc depth)
                                                    :max-depth max-depth
                                                    :limit limit
                                                    :entry-count entry-count)]
                        (when (and (seq subtree) (not (map? subtree)))
                          (let [subtree-lines (str/split-lines subtree)
                                indented-lines (map #(str indent "  " %) subtree-lines)]
                            (.append result (str (str/join "\n" indented-lines) "\n"))))))

                    (recur (rest remaining)))))
              ;; Process files
              remaining-files (process-entries files result indent entry-count limit)]
          ;; Add truncation message
          (add-truncation-message result indent (+ remaining-dirs remaining-files)))

        (.toString result))
      {:error (str path " is not a valid directory")})))
