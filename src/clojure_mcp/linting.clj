(ns clojure-mcp.linting
  (:require
   [clj-kondo.core :as kondo]
   [clj-kondo.impl.parser :as parser]
   [clojure.string :as string])
  (:import (clojure.lang ExceptionInfo)))

(defn lint
  "Lints Clojure code string using clj-kondo.
   Returns nil if no issues found, or map with :report and :error? keys.

   Options:
   - lang: Language type (:clj, :cljs, or :cljc)"
  ([form-str] (lint form-str {}))
  ([form-str {:keys [lang]}]
   (let [config {:ignore [:invalid-arity
                          :unresolved-symbol
                          :clj-kondo-config
                          :deprecated-var
                          :deprecated-namespace
                          :deps.edn
                          :bb.edn-undefined-task
                          :bb.edn-cyclic-task-dependency
                          :bb.edn-unexpected-key
                          :bb.edn-task-missing-docstring
                          :docstring-blank
                          :docstring-no-summary
                          :docstring-leading-trailing-whitespace
                          :file
                          :reduce-without-init
                          :line-length
                          :missing-docstring
                          :namespace-name-mismatch
                          :non-arg-vec-return-type-hint
                          :private-call
                          :redefined-var
                          :redundant-ignore
                          :schema-misplaced-return
                          :java-static-field-call
                          :unused-alias
                          ;; :unused-binding ;; <-- Removed this line to enable the warning
                          :unused-import
                          :unresolved-namespace
                          :unresolved-symbol
                          :unresolved-var
                          :unused-namespace
                          :unused-private-var
                          :unused-referred-var
                          :use]}
         lint-opts (cond-> {:lint ["-"] :config config}
                     lang (assoc :lang lang))
         res (with-in-str form-str
               (kondo/run! lint-opts))]
     (when (not-empty (:findings res))
       {:report (with-out-str
                  (kondo/print! res))
        :error? (some-> res :summary :error (> 0))}))))

(defn lint-delims [str]
  (try
    (parser/parse-string str)
    ;; linting passes
    false
    (catch ExceptionInfo e
      (if-let [findings (:findings (ex-data e))]
        {:report
         (some->> findings
                  not-empty
                  (map (fn [{:keys [row col message]}]
                         (format "<input>:%d:%d: Error: %s" row col message)))
                  (string/join "\n"))
         :error? true}
        (throw e)))))

(defn format-lint-warnings
  "Formats lint warnings into a more readable string.
   Takes the result from the lint function and returns a formatted string."
  [lint-result]
  (if (nil? lint-result)
    "No linting issues found."
    (let [report (:report lint-result)
          is-error (:error? lint-result)
          severity (if is-error "errors" "warnings")]
      (str "Code has linting " severity ":\n\n" report))))

(defn count-forms [code-str]
  (count (:children (parser/parse-string code-str))))

