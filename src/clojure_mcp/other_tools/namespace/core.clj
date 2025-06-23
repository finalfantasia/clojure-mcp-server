(ns clojure-mcp.other-tools.namespace.core
  "Core implementation for namespace-related tools.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.string :as string]))

(defn get-current-namespace
  "Returns the current namespace from the nREPL client's state.
   
   Parameters:
   - nrepl-client: The nREPL client to use
   
   Returns a map with:
   - :namespace - The current namespace as a string, or nil if not available
   - :error - Set to true if no current namespace was found"
  [nrepl-client]
  (let [current-ns (some-> nrepl-client
                           (nrepl/current-ns
                            (nrepl/eval-session nrepl-client)))]
    (if current-ns
      {:namespace current-ns :error false}
      {:error true :message "No current namespace found"})))

(defn get-all-namespaces
  "Returns a list of all currently loaded namespaces.
   
   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - eval-fn: A function that takes a client and code string, and returns a result string
   
   Returns a map with:
   - :namespaces - A vector of namespace strings sorted alphabetically
   - :error - Set to true if there was an error during evaluation"
  [nrepl-client eval-fn]
  (let [code "(map str (sort (map ns-name (all-ns))))"
        result-str (eval-fn nrepl-client code)]
    (if result-str
      (try
        (let [namespaces (read-string result-str)]
          {:namespaces (vec namespaces) :error false})
        (catch Exception e
          {:error true :message (str "Error parsing namespaces: " (.getMessage e))}))
      {:error true :message "Error retrieving namespaces"})))

(defn get-vars-in-namespace
  "Returns metadata for all public vars in a given namespace.
   
   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - eval-fn: A function that takes a client and code string, and returns a result string
   - namespace: The namespace to list vars from (as a string)
   
   Returns a map with:
   - :vars - A vector of maps containing metadata for each var
   - :error - Set to true if there was an error during evaluation"
  [nrepl-client eval-fn namespace]
  (let [ns-str (string/trim namespace)
        code (pr-str `(when-let [ns-obj# (find-ns (symbol ~ns-str))]
                        (->> (ns-publics ns-obj#)
                             vals ;; Get the var objects
                             (map meta) ;; Get metadata for each var
                             (map #(select-keys % [:arglists :doc :name :ns])) ;; Select desired keys
                             (map #(update % :ns str))
                             (sort-by :name) ;; Sort by name for consistent order
                             vec))) ;; Convert to vector
        result-str (eval-fn nrepl-client code)]
    (cond
      ;; Case 1: nREPL evaluation failed entirely
      (nil? result-str)
      {:error true :message "Error evaluating code to list vars."}

      ;; Case 2: Try to parse the result
      :else
      (try
        (let [result-val (read-string result-str)]
          (if result-val
            {:vars result-val :error false}
            {:error true :message (str "Namespace '" ns-str "' not found.")}))
        (catch Exception e
          {:error true :message (str "Error parsing result: " (.getMessage e))})))))
