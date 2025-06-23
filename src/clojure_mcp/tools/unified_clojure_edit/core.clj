(ns clojure-mcp.tools.unified-clojure-edit.core
  "Core utility functions for unified Clojure code editing.
   This namespace provides pattern-based form finding and editing
   without any MCP-specific code."
  (:require
   [clojure-mcp.sexp.match :as match]
   [clojure-mcp.tools.form-edit.core :as form-edit]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]))

;; Re-export common utilities from form-edit.core
(def format-source-string form-edit/format-source-string)
(def load-file-content form-edit/load-file-content)
(def save-file-content form-edit/save-file-content)
(def zloc-offsets form-edit/zloc-offsets)

(defn find-pattern-match
  "Finds a pattern match in Clojure source code.

   Arguments:
   - zloc: Source code zipper
   - pattern-str: Pattern to match with wildcards (? and *)

   Returns:
   - Map with :zloc pointing to the matched form or nil if not found"
  [zloc pattern-str]
  (let [pattern-sexpr (z/sexpr (z/of-string pattern-str))]
    (if-let [match-loc (match/find-match* pattern-sexpr zloc)]
      {:zloc match-loc}
      nil)))

(defn edit-matched-form
  "Edits a form matched by a pattern.

   Arguments:
   - zloc: Source code zipper
   - pattern-str: Pattern that matches the target form
   - content-str: New content to replace/insert
   - edit-type: Operation to perform (:replace, :insert-before, :insert-after)

   Returns:
   - Map with :zloc pointing to the edited form, or nil if match not found"
  [zloc pattern-str content-str edit-type]
  (if-let [match-result (find-pattern-match zloc pattern-str)]
    (let [match-loc (:zloc match-result)
          content-node (p/parse-string-all content-str)
          updated-loc (case edit-type
                        :replace
                        (z/replace match-loc content-node)
                        :insert_before
                        (-> match-loc
                            (z/insert-left (p/parse-string-all "\n\n"))
                            z/left
                            (z/insert-left content-node)
                            z/left) ; Move to the newly inserted node

                        :insert_after
                        (-> match-loc
                            (z/insert-right (p/parse-string-all "\n\n"))
                            z/right
                            (z/insert-right content-node)
                            z/right))] ; Move to the newly inserted node
      {:zloc updated-loc})
    nil))
