(ns clojure-mcp.tools.eval.core
  "Core implementation for the eval tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.linting :as linting]
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.sexp.paren-utils :as paren-utils]
   [clojure.string :as string]
   [clojure.tools.logging :as log]))

;; Eval results formatting
;; The goal is to make it clear for the LLM to understand
(defn partition-outputs [outputs]
  (when (not-empty outputs)
    (let [[non-val-parts [val & xs]] (split-with #(not= :value (first %)) outputs)]
      (cons (cond-> (vec non-val-parts)
              val (conj val))
            (partition-outputs xs)))))

(defn format-value [[k v]]
  (string/trim-newline
   (if (= k :value)
     (str "=> " v (if (<= nrepl/truncation-length (count v))
                    " ... RESULT TRUNCATED"
                    ""))
     (str v))))

(defn format-eval-outputs [outputs]
  (->> outputs
       (map format-value)
       (string/join "\n")))

(defn partition-and-format-outputs [outputs]
  (interpose "*===============================================*"
             (mapv format-eval-outputs (partition-outputs outputs))))

(defn repair-code
  [code]
  (let [linted (linting/lint code)]
    (if (and linted (:error? linted) (paren-utils/has-delimiter-errors? linted))
      (or (paren-utils/parinfer-repair code) code)
      code)))

(defn evaluate-code
  "Evaluates Clojure code using the nREPL client.

   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - opts: A map of options:
     - :code The Clojure code to evaluate as a string
     - :timeout_ms the timeout in milliseconds

   Returns:
   - A map with :outputs (raw outputs), :error (boolean flag)"
  [nrepl-client opts]
  (let [{:keys [code timeout_ms session]} opts
        timeout-ms (or timeout_ms 20000)
        outputs (atom [])
        error-occurred (atom false)
        form-str code
        linted (linting/lint form-str)
        add-output! (fn [prefix value] (swap! outputs conj [prefix value]))
        result-promise (promise)]

    ;; Add linter output if present
    (when linted
      (add-output! :lint (:report linted))
      (when (:error? linted)
        (reset! error-occurred true)))

    ;; If linter found critical errors, return early
    (if @error-occurred
      {:outputs @outputs
       :error true}

      ;; Otherwise, evaluate the code
      (do
        ;; Push to eval history if available
        (when-let [state (::nrepl/state nrepl-client)]
          (swap! state update :clojure-mcp.repl-tools/eval-history conj form-str))

        ;; Evaluate the code, using the namespace parameter if provided
        (try
          (nrepl/eval-code-msg
           nrepl-client form-str
           (if session {:session session} {})
           (->> identity
                (nrepl/out-err
                 #(add-output! :out %)
                 #(add-output! :err %))
                (nrepl/value #(add-output! :value %))
                (nrepl/done (fn [_]
                              (deliver result-promise
                                       {:outputs @outputs
                                        :error @error-occurred})))
                (nrepl/error (fn [{:keys [exception]}]
                               (reset! error-occurred true)
                               (add-output! :err exception)
                               (deliver result-promise
                                        {:outputs @outputs
                                         :error true})))))
          (catch Exception e
            ;; prevent connection errors from confusing the LLM
            (log/error e "Error when trying to eval on the nrepl connection")
            (throw
             (ex-info
              (str "Internal Error: Unable to reach the nREPL "
                   "thus we are unable to execute the bash command.")
              {:error-type :connection-error}
              e))))

        ;; Wait for the result and return it
        (let [tmb (Object.)
              res (deref result-promise timeout-ms tmb)]
          (if-not (= tmb res)
            res
            (do
              (nrepl/interrupt nrepl-client)
              {:outputs [[:err (str "Eval timed out after " timeout-ms "ms.")]
                         [:err "Perhaps, you had an infinite loop or an eval that ran too long."]]
               :error true})))))))

(defn evaluate-with-repair
  "Evaluates Clojure code with automatic repair of delimiter errors.
   First attempts to repair any delimiter errors in the code,
   then evaluates the repaired code if successful.

   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - opts: A map of options:
     - :code The Clojure code to evaluate as a string
     - :session Optional session
   Returns:
   - A map with :outputs (raw outputs), :error (boolean flag), :repaired (boolean flag)"
  [nrepl-client opts]
  (let [{:keys [code]} opts
        repaired-code (repair-code code)
        repaired? (not= repaired-code code)
        opts (assoc opts :code repaired-code)]
    (assoc (evaluate-code nrepl-client opts)
           :repaired repaired?)))

(comment
  ;; === Examples of using the eval core functionality directly ===

  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)

  ;; Test simple expression evaluation with options map
  (evaluate-code @client-atom {:code "(+ 1 2)"})

  ;; Test with namespace option
  (evaluate-code @client-atom {:code "(str *ns*)" :namespace "clojure.string"})

  ;; Test multi-expression evaluation with options map
  (evaluate-code @client-atom {:code "(println \"Hello World\")\n(+ 3 4)"})

  ;; Test evaluation with error with options map
  (evaluate-code @client-atom {:code "(/ 1 0)"})

  ;; Test evaluation with linter warning with options map
  (evaluate-code @client-atom {:code "(let [unused 1] (+ 2 3))"})

  ;; Test backward compatibility with string parameter
  (evaluate-code @client-atom "(+ 1 2)") ;; Will be converted to {:code "(+ 1 2)"}

  ;; Test the repair-code function directly
  (repair-code "(defn hello [name] (println name)") ;; Missing closing paren
  (repair-code "(defn hello [name] (println name)))") ;; Extra closing paren
  (repair-code "(defn hello [123] (println name))") ;; Non-repairable syntax error
  (repair-code "(defn hello [name] (println \"Hello))") ;; Non-repairable string error

  ;; Test the evaluate-with-repair function with options map
  (evaluate-with-repair @client-atom {:code "(+ 1 2)"}) ;; Well-formed code

  ;; Should auto-repair and evaluate successfully
  (evaluate-with-repair @client-atom {:code "(defn hello [name] (println name)"})
  (evaluate-with-repair @client-atom {:code "(defn hello [name] (println name)))"})

  ;; Test with namespace
  (evaluate-with-repair @client-atom {:code "(str *ns*)" :namespace "clojure.string"})
  (evaluate-with-repair @client-atom {:code "(join \", \" [\"a\" \"b\" \"c\"])" :namespace "clojure.string"})

  ;; Test backward compatibility with string parameter
  (evaluate-with-repair @client-atom "(+ 1 2)") ;; Will be converted to {:code "(+ 1 2)"}

  ;; Should detect non-repairable errors
  (evaluate-with-repair @client-atom {:code "(defn hello [123] (println name))"})
  (evaluate-with-repair @client-atom {:code "(defn hello [name] (println \"Hello))"})

  ;; Cleanup
  (clojure-mcp.nrepl/stop-polling @client-atom))
