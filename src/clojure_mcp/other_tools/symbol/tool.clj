(ns clojure-mcp.other-tools.symbol.tool
  "Implementation of symbol-related tools using the tool-system multimethod approach."
  (:require
   [clojure-mcp.other-tools.symbol.core :as core]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.pprint :as pprint]))

;; Factory functions for creating tool configurations

(defn create-symbol-completions-tool
  "Creates the symbol-completions tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :symbol-completions
   :nrepl-client-atom nrepl-client-atom})

(defn create-symbol-metadata-tool
  "Creates the symbol-metadata tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :symbol-metadata
   :nrepl-client-atom nrepl-client-atom})

(defn create-symbol-documentation-tool
  "Creates the symbol-documentation tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :symbol-documentation
   :nrepl-client-atom nrepl-client-atom})

(defn create-source-code-tool
  "Creates the source-code tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :source-code
   :nrepl-client-atom nrepl-client-atom})

(defn create-symbol-search-tool
  "Creates the symbol-search tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :symbol-search
   :nrepl-client-atom nrepl-client-atom})

;; This helper is no longer needed as we directly use nrepl functions

;; ===== Symbol Completions Tool Implementation =====

(defmethod tool-system/tool-name :symbol-completions [_]
  "symbol_completions")

(defmethod tool-system/tool-description :symbol-completions [_]
  "Provides a list of completion candidates for the current namespace. For example, if you provide the prefix of a symbol like 'map', this will return a list of possible symbols that could start with that prefix in the current namespace. This is not an exhaustive list; some completions may be missing.")

(defmethod tool-system/tool-schema :symbol-completions [_]
  {:type :object
   :properties {:prefix {:type :string
                         :description "The symbol prefix to complete, or empty string for all available symbols."}}
   :required [:prefix]})

(defmethod tool-system/validate-inputs :symbol-completions [_ inputs]
  (let [{:keys [prefix]} inputs]
    (when-not (contains? inputs :prefix)
      (throw (ex-info "Missing required parameter: prefix" {:inputs inputs})))
    {:prefix (or prefix "")})) ;; Allow empty prefix, which means "list all"

(defmethod tool-system/execute-tool :symbol-completions [{:keys [nrepl-client-atom]} inputs]
  (core/get-symbol-completions @nrepl-client-atom (:prefix inputs)))

(defmethod tool-system/format-results :symbol-completions [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result (:completions result)
     :error false}))

;; ===== Symbol Metadata Tool Implementation =====

(defmethod tool-system/tool-name :symbol-metadata [_]
  "symbol_metadata")

(defmethod tool-system/tool-description :symbol-metadata [_]
  "Returns the complete metadata for the symbol. The most important data is likely to be the :arglists that shows the shape of the arguments that the function takes, and :doc which holds the docstring for the function/var.")

(defmethod tool-system/tool-schema :symbol-metadata [_]
  {:type :object
   :properties {:symbol {:type :string
                         :description "The name of the symbol to look up."}}
   :required [:symbol]})

(defmethod tool-system/validate-inputs :symbol-metadata [_ inputs]
  (let [{:keys [symbol]} inputs]
    (when-not symbol
      (throw (ex-info "Missing required parameter: symbol" {:inputs inputs})))
    {:symbol symbol}))

(defmethod tool-system/execute-tool :symbol-metadata [{:keys [nrepl-client-atom]} inputs]
  (core/get-symbol-metadata @nrepl-client-atom (:symbol inputs)))

(defmethod tool-system/format-results :symbol-metadata [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result [(with-out-str (pprint/pprint (:metadata result)))]
     :error false}))

;; ===== Symbol Documentation Tool Implementation =====

(defmethod tool-system/tool-name :symbol-documentation [_]
  "symbol_documentation")

(defmethod tool-system/tool-description :symbol-documentation [_]
  "Returns the documentation for the symbol. Extracts the doc string and includes the function's arglists from the symbol metadata.")

(defmethod tool-system/tool-schema :symbol-documentation [_]
  {:type :object
   :properties {:symbol {:type :string
                         :description "The name of the symbol to look up."}}
   :required [:symbol]})

(defmethod tool-system/validate-inputs :symbol-documentation [_ inputs]
  (let [{:keys [symbol]} inputs]
    (when-not symbol
      (throw (ex-info "Missing required parameter: symbol" {:inputs inputs})))
    {:symbol symbol}))

(defmethod tool-system/execute-tool :symbol-documentation [{:keys [nrepl-client-atom]} inputs]
  (core/get-symbol-documentation @nrepl-client-atom (:symbol inputs)))

(defmethod tool-system/format-results :symbol-documentation [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result [(str (:arglists result)
                   "\n"
                   (:doc result))]
     :error false}))

;; ===== Source Code Tool Implementation =====

(defmethod tool-system/tool-name :source-code [_]
  "source_code")

(defmethod tool-system/tool-description :source-code [_]
  "Returns the source code for a given symbol using clojure.repl/source. Provide a string representing the symbol, e.g. 'map' or 'clojure.core/map'.")

(defmethod tool-system/tool-schema :source-code [_]
  {:type :object
   :properties {:symbol {:type :string
                         :description "The name of the symbol to get source for."}}
   :required [:symbol]})

(defmethod tool-system/validate-inputs :source-code [_ inputs]
  (let [{:keys [symbol]} inputs]
    (when-not symbol
      (throw (ex-info "Missing required parameter: symbol" {:inputs inputs})))
    {:symbol symbol}))

(defmethod tool-system/execute-tool :source-code [{:keys [nrepl-client-atom]} inputs]
  (core/get-source-code @nrepl-client-atom (:symbol inputs)))

(defmethod tool-system/format-results :source-code [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result [(:source result)]
     :error false}))

;; ===== Symbol Search Tool Implementation =====

(defmethod tool-system/tool-name :symbol-search [_]
  "symbol_search")

(defmethod tool-system/tool-description :symbol-search [_]
  "Returns a sequence of all public definitions whose names contain the given search string in all currently loaded namespaces using clojure.repl/apropos.")

(defmethod tool-system/tool-schema :symbol-search [_]
  {:type :object
   :properties {:search-str {:type :string
                             :description "The string to search for in symbol names."}}
   :required [:search-str]})

(defmethod tool-system/validate-inputs :symbol-search [_ inputs]
  (let [{:keys [search-str]} inputs]
    (when-not search-str
      (throw (ex-info "Missing required parameter: search-str" {:inputs inputs})))
    {:search-str search-str}))

(defmethod tool-system/execute-tool :symbol-search [{:keys [nrepl-client-atom]} inputs]
  (core/search-symbols @nrepl-client-atom (:search-str inputs)))

(defmethod tool-system/format-results :symbol-search [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result (:matches result)
     :error false}))

;; ===== Backward Compatibility Functions =====

(defn symbol-completions-tool
  "Returns the registration map for the symbol-completions tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-symbol-completions-tool nrepl-client-atom)))

(defn symbol-metadata-tool
  "Returns the registration map for the symbol-metadata tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-symbol-metadata-tool nrepl-client-atom)))

(defn symbol-documentation-tool
  "Returns the registration map for the symbol-documentation tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-symbol-documentation-tool nrepl-client-atom)))

(defn source-code-tool
  "Returns the registration map for the source-code tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-source-code-tool nrepl-client-atom)))

(defn symbol-search-tool
  "Returns the registration map for the symbol-search tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-symbol-search-tool nrepl-client-atom)))