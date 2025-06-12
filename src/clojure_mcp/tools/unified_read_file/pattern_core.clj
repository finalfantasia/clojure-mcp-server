(ns clojure-mcp.tools.unified-read-file.pattern-core
  "Core implementation for pattern-based Clojure file exploration."
  (:require
   [clojure-mcp.tools.form-edit.core :as form-edit]
   [rewrite-clj.zip :as z]))

(defn valid-form-to-include?
  "Checks if a form should be included in analysis, excluding comments unless specified."
  [zloc include-comments]
  (try
    (when zloc
      (let [tag (z/tag zloc)]
        (not (or (= tag :uneval)
                 (= tag :whitespace)
                 (= tag :newline)
                 (and (not include-comments) (= tag :comment))))))
    (catch Exception _
      false)))

(defn extract-form-name
  "Extracts name from a form, e.g., 'foo' from (defn foo [x] ...).
   For defmethod forms, includes the dispatch value, e.g., 'area :square'."
  [sexpr]
  (try
    (when (and (seq? sexpr) (> (count sexpr) 1))
      (cond
        ;; Special handling for defmethod forms
        (and (symbol? (first sexpr))
             (= (name (first sexpr)) "defmethod")
             (>= (count sexpr) 3))
        (let [method-sym (second sexpr)
              method-name (if (and (symbol? method-sym) (namespace method-sym))
                            (str (namespace method-sym) "/" (name method-sym))
                            (name method-sym))
              dispatch-val (nth sexpr 2)
              dispatch-str (pr-str dispatch-val)]
          (str method-name " " dispatch-str))

        ;; Regular form handling
        (symbol? (second sexpr))
        (name (second sexpr))

        :else nil))
    (catch Exception _
      nil)))

(defn collect-top-level-forms
  "Collects metadata about all top-level forms in a Clojure file."
  [file-path include-comments]
  (try
    (let [file-content (slurp file-path)
          zloc (z/of-string file-content)]
      (loop [loc zloc
             forms []]
        (if (nil? loc)
          forms
          (if (valid-form-to-include? loc include-comments)
            (let [sexpr (try (z/sexpr loc) (catch Exception _ nil))
                  form-name (extract-form-name sexpr)
                  form-type (when (and (seq? sexpr) (symbol? (first sexpr)))
                              (name (first sexpr)))
                  form-content (z/string loc)]
              (recur (try (z/right loc) (catch Exception _ nil))
                     (if form-name
                       (conj forms {:name form-name
                                    :type form-type
                                    :content form-content})
                       forms)))
            (recur (try (z/right loc) (catch Exception _ nil)) forms)))))
    (catch Exception e
      (throw (ex-info (str "Error collecting forms: " (.getMessage e))
                      {:file-path file-path})))))

(defn filter-forms-by-pattern
  "Filters forms based on name and/or content regex patterns.
   For defmethod forms, matches against the combined 'method-name dispatch-value' string."
  [forms name-pattern content-pattern]
  (let [name-regex (when (and name-pattern (not= name-pattern ""))
                     (try (re-pattern name-pattern)
                          (catch Exception e
                            (throw (ex-info (str "Invalid name pattern regex: " (.getMessage e))
                                            {:pattern name-pattern})))))
        content-regex (when (and content-pattern (not= content-pattern ""))
                        (try (re-pattern content-pattern)
                             (catch Exception e
                               (throw (ex-info (str "Invalid content pattern regex: " (.getMessage e))
                                               {:pattern content-pattern})))))
        matches (->> forms
                     (filter (fn [{:keys [name content]}]
                               (or (and name-regex name (re-find name-regex name))
                                   (and content-regex content (re-find content-regex content)))))
                     (map :name))]
    {:matches matches
     :pattern-info {:name-pattern name-pattern
                    :content-pattern content-pattern
                    :match-count (count matches)}}))

(defn generate-pattern-based-file-view
  "Generates collapsed view with pattern-based expansion of Clojure files."
  [file-path name-pattern content-pattern]
  (let [forms (collect-top-level-forms file-path false)
        result (filter-forms-by-pattern forms name-pattern content-pattern)
        matching-names (:matches result)]
    {:matches matching-names
     :pattern-info (:pattern-info result)}))

(comment
  (def tmp-path "/Users/bruce/workspace/llempty/clojure-mcp/src/clojure_mcp/tools/form_edit/tool.clj")

  (-> (collect-top-level-forms tmp-path false)
      (filter-forms-by-pattern "validate" nil)
      :matches
      (->>
       (form-edit/generate-collapsed-file-view tmp-path))
      println))




