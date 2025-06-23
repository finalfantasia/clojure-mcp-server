(ns clojure-mcp.tools.dispatch-agent.core
  "Core implementation for the dispatch agent tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
   #_[clojure-mcp.tools.scratch-pad.tool :as scratch-pad-tool]
   [clojure-mcp.agent.langchain :as chain]
   [clojure-mcp.tools.directory-tree.tool :as directory-tree-tool]
   [clojure-mcp.tools.glob-files.tool :as glob-files-tool]
   [clojure-mcp.tools.grep.tool :as grep-tool]
   [clojure-mcp.tools.project.tool :as project-tool]
   [clojure-mcp.tools.think.tool :as think-tool]
   [clojure-mcp.tools.unified-read-file.tool :as read-file-tool]
   [clojure.string :as string]
   [clojure.tools.logging :as log])
  (:import
   (clojure_mcp.agent.langchain AiService)))

(declare system-message)

(defn create-ai-service
  "Creates an AI service for doing read only tasks.
   
   Args:
   - nrepl-client-atom: Required nREPL client atom
   - model: Optional pre-built langchain model. If nil, uses chain/agent-model"
  ([nrepl-client-atom] (create-ai-service nrepl-client-atom nil))
  ([nrepl-client-atom model]
   (try
     (when-let [selected-model (or model
                                   (some-> (chain/agent-model)
                                           (.build)))]
       (let [memory (chain/chat-memory 300)
             ai-service-data {:memory memory
                              :model selected-model
                              :tools
                              (mapv
                               #(% nrepl-client-atom)
                               [read-file-tool/unified-read-file-tool
                                directory-tree-tool/directory-tree-tool
                                grep-tool/grep-tool
                                glob-files-tool/glob-files-tool
                                project-tool/inspect-project-tool
                                think-tool/think-tool
                                #_scratch-pad-tool/scratch-pad-tool])
                              :system-message system-message}
             service (-> (chain/create-service AiService ai-service-data)
                         (.build))]
         (assoc ai-service-data
                :service service)))
     (catch Exception e
       (log/error e "Failed to create dispatch_agent AI service")
       (throw e)))))

(defn get-ai-service
  "Gets or creates an AI service. Uses a simple cache in nrepl-client-atom.
   
   Args:
   - nrepl-client-atom: Required nREPL client atom  
   - model: Optional pre-built langchain model"
  ([nrepl-client-atom] (get-ai-service nrepl-client-atom nil))
  ([nrepl-client-atom model]
   (or (::ai-service @nrepl-client-atom)
       (when-let [ai (create-ai-service nrepl-client-atom model)]
         (swap! nrepl-client-atom assoc ::ai-service ai)
         ai))))

(defn dispatch-agent
  "Dispatches an agent with the given prompt. The agent will only have access to read tools.
   Returns a string response from the agent.
   
   Args:
   - context: Map containing :nrepl-client-atom and optional :model
   - prompt: String prompt to send to the agent"
  [{:keys [nrepl-client-atom model]} prompt]
  (if (string/blank? prompt)
    {:result "Error: Cannot process empty prompt"
     :error true}
    (if-let [ai-service (get-ai-service nrepl-client-atom model)]
      ;; Clear memory for stateless behavior
      (do
        (.clear (:memory ai-service))
        (let [result (.chat (:service ai-service) prompt)]
          {:result result
           :error false}))
      {:result "ERROR: No model configured for this agent."
       :error true})))

(defn validate-dispatch-agent-inputs
  "Validates inputs for the dispatch-agent function"
  [inputs]
  (cond
    (nil? inputs)
    (throw (ex-info "Missing inputs" {:error-details ["No input parameters provided"]}))

    (nil? (:prompt inputs))
    (throw (ex-info "Missing required parameter: prompt"
                    {:error-details ["The 'prompt' parameter is required"]}))

    (not (string? (:prompt inputs)))
    (throw (ex-info "Parameter 'prompt' must be a string"
                    {:error-details [(str "Got: " (type (:prompt inputs)))]}))

    :else
    inputs))

(comment
  ;; Example usage with custom model
  (let [user-dir (System/getProperty "user.dir")
        client-atom (atom {:clojure-mcp.config/config
                           {:nrepl-user-dir user-dir
                            :allowed-directories [user-dir]}})
       ;; Use default model
        #_#_ai-default (create-ai-service client-atom)
       ;; Use custom model
        custom-model (->
                      (chain/create-gemini-model "gemini-2.5-flash-preview-05-20")
                      #_(chain/create-anthropic-model "claude-3-7-sonnet-20250219")
                      (.build))
        ai-custom (create-ai-service client-atom custom-model)]

   ;; Test with default model
    #_(.chat (:service ai-default) "find langchain integration code")

   ;; Test with custom model
    (.chat (:service ai-custom) "find the langchain integration code")))

(def system-message
  "You are an agent for a Clojure Coding Assistant. Given the user's prompt, you should use the tools available to you to answer the user's question.

Notes:
1. IMPORTANT: You should be concise, direct, and to the point, since your responses will be displayed on a command line interface. Answer the user's question directly, without elaboration, explanation, or details. One word answers are best. Avoid introductions, conclusions, and explanations. You MUST avoid text before/after your response, such as \"The answer is <answer>.\", \"Here is the content of the file...\" or \"Based on the information provided, the answer is...\" or \"Here is what I will do next...\".
2. When relevant, share file names and code snippets relevant to the query
3. Any file paths you return in your final response MUST be absolute. DO NOT use relative paths.")
