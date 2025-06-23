(ns clojure-mcp.other-tools.symbol.core
  "Core implementation for symbol-related tools.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure.string :as string]))

(defn get-symbol-completions
  "Returns completions for a given prefix in the current namespace.
   
   Parameters:
   - nrepl-client: The nREPL client to use
   - prefix: The symbol prefix to complete (or empty string for all symbols)
   
   Returns a map with:
   - :completions - A vector of completion candidates
   - :error - Set to true if there was an error during retrieval"
  [nrepl-client prefix]
  (try
    ;; Use the completions function directly from clojure-mcp.nrepl
    (let [completions-raw (clojure-mcp.nrepl/completions nrepl-client prefix)
          candidates (mapv :candidate completions-raw)]
      {:completions candidates :error false})
    (catch Exception e
      {:error true :message (str "Error retrieving completions: " (.getMessage e))})))

(defn get-symbol-metadata
  "Returns complete metadata for a given symbol.
   
   Parameters:
   - nrepl-client: The nREPL client to use
   - symbol-name: The name of the symbol to look up (as a string)
   
   Returns a map with:
   - :metadata - The complete metadata for the symbol
   - :error - Set to true if there was an error or the symbol was not found"
  [nrepl-client symbol-name]
  (try
    (if-let [metadata (nrepl/lookup nrepl-client symbol-name)]
      {:metadata metadata :error false}
      {:error true :message (str "Symbol '" symbol-name "' not found")})
    (catch Exception e
      {:error true :message (str "Error retrieving metadata: " (.getMessage e))})))

(defn get-symbol-documentation
  "Returns the documentation and arglists for a given symbol.
   
   Parameters:
   - nrepl-client: The nREPL client to use
   - symbol-name: The name of the symbol to look up (as a string)
   
   Returns a map with:
   - :arglists - The argument lists for the symbol (if it's a function)
   - :doc - The docstring for the symbol
   - :error - Set to true if there was an error or the symbol was not found"
  [nrepl-client symbol-name]
  (try
    (if-let [metadata (nrepl/lookup nrepl-client symbol-name)]
      {:arglists (:arglists metadata)
       :doc (:doc metadata)
       :error false}
      {:error true :message (str "Symbol '" symbol-name "' not found")})
    (catch Exception e
      {:error true :message (str "Error retrieving documentation: " (.getMessage e))})))

(defn get-source-code
  "Returns the source code for a given symbol.
   
   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - symbol-name: The name of the symbol to get source for (as a string)
   
   Returns a map with:
   - :source - The source code as a string
   - :error - Set to true if there was an error during evaluation"
  [nrepl-client symbol-name]
  (let [code (pr-str `(clojure.repl/source-fn (symbol ~symbol-name)))
        result-str (nrepl/tool-eval-code nrepl-client code)]
    (if result-str
      (try
        (let [source (read-string result-str)]
          (if (nil? source)
            {:error true :message (str "Source not found for '" symbol-name "'")}
            {:source source :error false}))
        (catch Exception e
          {:error true :message (str "Error parsing source: " (.getMessage e))}))
      {:error true :message (str "Error retrieving source for '" symbol-name "'")})))

(defn search-symbols
  "Searches for symbols containing the given string across all namespaces.
   
   Parameters:
   - nrepl-client: The nREPL client to use for evaluation
   - search-str: The string to search for in symbol names
   
   Returns a map with:
   - :matches - A vector of matching symbol names
   - :error - Set to true if there was an error during evaluation"
  [nrepl-client search-str]
  (let [code (pr-str `(clojure.repl/apropos ~search-str))
        result-str (nrepl/tool-eval-code nrepl-client code)]
    (if result-str
      (try
        (let [matches (some->> (read-string result-str)
                               (map str)
                               (remove #(string/starts-with? % "cider.nrepl"))
                               vec)]
          (if (empty? matches)
            {:matches ["No matches found"] :error false}
            {:matches matches :error false}))
        (catch Exception e
          {:error true :message (str "Error parsing search results: " (.getMessage e))}))
      {:error true :message (str "Error searching for '" search-str "'")})))