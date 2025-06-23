(ns clojure-mcp.sexp.paren-utils
  (:require
   [clojure-mcp.linting :as linting]
   [clojure.string :as string]
   [rewrite-clj.node :as node]
   [rewrite-clj.parser :as parser])
  (:import (com.oakmac.parinfer Parinfer)))

;; Tokenizer that breaks code into expressions and delimiter tokens
(defn tokenize-code
  "Tokenize Clojure code into valid expressions and individual tokens.
   Uses parse-string to extract complete valid expressions."
  [code-str]
  (loop [remaining code-str
         tokens []]
    (if (string/blank? remaining)
      tokens
      (let [result
            (try
              (let [expr (parser/parse-string remaining)
                    expr-str (node/string expr)]
                {:success true
                 :expr-str expr-str
                 :token {:type :expression, :value expr-str}})
              (catch Exception _
                (let [first-char (first remaining)
                      delimiters #{\( \) \[ \] \{ \}}]
                  {:success false
                   :token (if (contains? delimiters first-char)
                            {:type :delimiter, :value first-char}
                            {:type :invalid, :value (str first-char)})})))]
        (if (:success result)
          (recur (subs remaining (count (:expr-str result)))
                 (conj tokens (:token result)))
          (recur (subs remaining 1)
                 (conj tokens (:token result))))))))

(def delim-map {\( \) \[ \] \{ \}})

(def open-delim #{\( \[ \{})

(defn invert-delim [{:keys [type value] :as x}]
  {:pre [(= :delimiter type) (open-delim value)]}
  (update x :value delim-map))

(defn open-delim? [{:keys [type value] :as _x}]
  (when (= :delimiter type)
    (open-delim value)))

(defn fix-parens
  ([tokens]
   (fix-parens 0 nil [] [] tokens))
  ([extra-closes open-stack bads accum [tk & xs :as tokens]]
   (cond
     (and (empty? tokens) (empty? bads))
     {:success true
      :removed-closing-delims extra-closes
      :added-closing-delims (count open-stack)
      :result (concat accum (map invert-delim open-stack))}

     (empty? tokens)
     {:success false
      :extra-closes extra-closes
      :bad-code bads
      :partial-result accum}

     (open-delim? tk)
     (fix-parens extra-closes (cons tk open-stack) bads (conj accum tk) xs)

     (= :delimiter (:type tk)) ;; ignore closing delim
     (fix-parens (inc extra-closes) open-stack bads accum xs)

     (= :expression (:type tk))
     (fix-parens extra-closes open-stack bads (conj accum tk) xs)

     :else
     (fix-parens extra-closes open-stack (conj bads tk) accum xs))))

(defn repair-parens [code-str]
  (let [{:keys [success result removed-closing-delims added-closing-delims]}
        (fix-parens (tokenize-code code-str))]
    (when success
      {:repaired? true
       :form (string/join (map :value result))
       :message
       (cond-> nil
         (> removed-closing-delims 0)
         (str (format "Removed %s extra closing parentheses. " removed-closing-delims))
         (> added-closing-delims 0)
         (str (format "Added %s missing closing parentheses" added-closing-delims)))})))

(defn parinfer-repair [code-str]
  (let [res (Parinfer/indentMode code-str nil nil nil false)]
    (when (and (.success res)
               (not (:error? (linting/lint (.text res)))))
      (.text res))))

(def delimiter-error-patterns [#"Unmatched bracket"
                               #"Found an opening .* with no matching"
                               #"Expected a .* to match"
                               #"Mismatched bracket"
                               #"Unexpected EOF while reading"
                               #"Unmatched opening"
                               #"Unmatched closing"])

(defn has-delimiter-errors? [{:keys [report error?] :as lint-result}]
  (if (and lint-result error?)
    (boolean (some #(re-find % report) delimiter-error-patterns))
    false))

(defn code-has-delimiter-errors?
  "Returns true if the given Clojure code string has delimiter errors
   (unbalanced parentheses, brackets, braces, or string quotes).
   Returns false otherwise."
  [code-str]
  (let [lint-result (linting/lint-delims code-str)]
    (has-delimiter-errors? lint-result)))

#_(defn smart-repair [code-str]
    (if-let [parinfer-result (parinfer-repair code-str)]
      (if-let [lint-result (linting/lint parinfer-result)]
        {:repaired? true
         :form parinfer-result
         :message "Parenthesis repaired using parinfer"}
        ;; Parinfer produced code with semantic issues, try homegrown approach
        (or (repair-parens code-str)
            ;; If homegrown fails too, return parinfer result with a warning
            {:repaired? true
             :form parinfer-result
             :message "Warning: Fixed syntax but may have changed semantics"}))
      ;; Parinfer failed (unlikely), fall back to homegrown
      (repair-parens code-str)))

(comment
  (def code1 "(defn hello [name] (str \"Hello\" name)))")
  (repair-parens code1)
  (tokenize-code code1)
  (fix-parens (tokenize-code code1))
  (repair-parens code1)

  ;; => {:repaired? true, :form "(defn hello [name] (str \"Hello\" name))", :message "Removed 1 extra closing parentheses"}

  ;; Test with missing closing paren
  (def code2 "(defn hello [name] (str \"Hello\" name)")
  (tokenize-code code2)
  (fix-parens (tokenize-code code2))
  (repair-parens code2)
  (parinfer-repair code2)
  ;; => {:repaired? true, :form "(defn hello [name] (str \"Hello\" name))", :message "Added 1 missing closing parentheses"}

  ;; Test with complex case - both extra and missing parens
  (def code3 "(defn hello [name]
  (str \"Hello\" name)))
  (defn world []
    (println \"World\")")
  (tokenize-code code3)
  (fix-parens (tokenize-code code3))
  (repair-parens code3)
  (repair-parens code3)
  ;; => {:repaired? true, :form "(defn hello [name] (str \"Hello\" name)) (defn world [] (println \"World\"))",
  ;;     :message "Removed 1 extra closing parentheses and added 1 missing closing parentheses"}

  ;; Test with parens in string
  (def code4 "(println \"Hello (world)\")")
  (tokenize-code code4)
  (fix-parens (tokenize-code code4))
  (repair-parens code4))

  ;; => {:repaired? false, :form "(println \"Hello (world)\")", :message "No repair needed"}

