(ns clojure-mcp.other-tools.namespace.tool
  "Implementation of namespace-related tools using the tool-system multimethod approach."
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.other-tools.namespace.core :as core]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.string :as str]))

;; Factory functions for creating tool configurations

(defn create-current-namespace-tool
  "Creates the current-namespace tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :current-namespace
   :nrepl-client-atom nrepl-client-atom})

(defn create-list-namespaces-tool
  "Creates the list-namespaces tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :list-namespaces
   :nrepl-client-atom nrepl-client-atom})

(defn create-list-vars-tool
  "Creates the list-vars-in-namespace tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :list-vars-in-namespace
   :nrepl-client-atom nrepl-client-atom})

;; Common implementation of the eval function used across namespace tools
(defn eval-code-helper [client code]
  (nrepl/tool-eval-code client code))

;; ===== Current Namespace Tool Implementation =====

(defmethod tool-system/tool-name :current-namespace [_]
  "current_namespace")

(defmethod tool-system/tool-description :current-namespace [_]
  "Returns the current namespace. This tool is intended for the LLM agent to check the current evaluation context. Use this tool when the agent is uncertain about which namespace is active and needs to verify the context before proceeding with further code evaluations.")

(defmethod tool-system/tool-schema :current-namespace [_]
  {:type :object})

(defmethod tool-system/validate-inputs :current-namespace [_ _]
  {})

(defmethod tool-system/execute-tool :current-namespace [{:keys [nrepl-client-atom]} _]
  (core/get-current-namespace @nrepl-client-atom))

(defmethod tool-system/format-results :current-namespace [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result [(:namespace result)]
     :error false}))

;; ===== List Namespaces Tool Implementation =====

(defmethod tool-system/tool-name :list-namespaces [_]
  "clojure_list_namespaces")

(defmethod tool-system/tool-description :list-namespaces [_]
  "Returns a list of all currently loaded namespaces as strings. Use this to see what libraries are loaded and search their capabilities.")

(defmethod tool-system/tool-schema :list-namespaces [_]
  {:type :object})

(defmethod tool-system/validate-inputs :list-namespaces [_ _]
  {})

(defmethod tool-system/execute-tool :list-namespaces [{:keys [nrepl-client-atom]} _]
  (core/get-all-namespaces @nrepl-client-atom eval-code-helper))

(defmethod tool-system/format-results :list-namespaces [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    {:result (:namespaces result)
     :error false}))

;; ===== List Vars in Namespace Tool Implementation =====

(defmethod tool-system/tool-name :list-vars-in-namespace [_]
  "clojure_list_vars_in_namespace")

(defmethod tool-system/tool-description :list-vars-in-namespace [_]
  "Returns a list of maps, each containing metadata (:arglists, :doc, :name, :ns) for public vars defined in a given namespace. This can give you an overview of the functions that a namespace provides and how to use them.")

(defmethod tool-system/tool-schema :list-vars-in-namespace [_]
  {:type :object
   :properties {:namespace {:type :string
                            :description "The fully qualified name of the namespace (e.g. clojure.string)."}}
   :required [:namespace]})

(defmethod tool-system/validate-inputs :list-vars-in-namespace [_ inputs]
  (let [{:keys [namespace]} inputs]
    (when-not namespace
      (throw (ex-info "Missing required parameter: namespace" {:inputs inputs})))
    {:namespace namespace}))

(defmethod tool-system/execute-tool :list-vars-in-namespace [{:keys [nrepl-client-atom]} inputs]
  (core/get-vars-in-namespace @nrepl-client-atom eval-code-helper (:namespace inputs)))

(defmethod tool-system/format-results :list-vars-in-namespace [_ result]
  (if (:error result)
    {:result [(:message result)]
     :error true}
    (let [format-var (fn [var-meta]
                       (let [name-str (str (:name var-meta))
                             arglists-str (when-let [args (:arglists var-meta)]
                                            (str "  " (pr-str args)))
                             doc-str (when-let [doc (:doc var-meta)]
                                       (str "  " doc))
                             parts (cond-> [name-str]
                                     arglists-str (conj arglists-str)
                                     doc-str (conj doc-str))]
                         (str/join "\n" parts)))
          formatted-vars (mapv format-var (:vars result))
          final-output (if (seq formatted-vars)
                         (str/join "\n---\n" formatted-vars)
                         "No public vars found in this namespace.")]
      {:result [final-output]
       :error false})))

;; ===== Backward Compatibility Functions =====

(defn current-namespace-tool
  "Returns the registration map for the current-namespace tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-current-namespace-tool nrepl-client-atom)))

(defn list-namespaces-tool
  "Returns the registration map for the list-namespaces tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-list-namespaces-tool nrepl-client-atom)))

(defn list-vars-in-namespace-tool
  "Returns the registration map for the list-vars-in-namespace tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-list-vars-tool nrepl-client-atom)))
