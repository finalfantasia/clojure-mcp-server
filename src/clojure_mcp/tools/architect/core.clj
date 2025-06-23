(ns clojure-mcp.tools.architect.core
  "Core implementation for the architect tool.
   This namespace contains the pure functionality without any MCP-specific code."
  (:require
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
  "Creates an AI service for architecture-related tasks.
   
   Args:
   - nrepl-client-atom: Required nREPL client atom
   - model: Optional pre-built langchain model. If nil, uses chain/reasoning-agent-model with medium reasoning effort"
  ([nrepl-client-atom] (create-ai-service nrepl-client-atom nil))
  ([nrepl-client-atom model]
   (try
     (when-let [selected-model (or model
                                   (some-> (chain/reasoning-agent-model)
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
             service (-> (chain/create-service AiService
                                               ai-service-data)
                         (.build))]
         (log/info "AI service for architect successfully created")
         (assoc ai-service-data
                :service service)))
     (catch Exception e
       (log/error e "Failed to create AI service for architect")
       (throw e)))))

(defn get-ai-service [nrepl-client-atom model]
  (or (::ai-service @nrepl-client-atom)
      (when-let [ai (create-ai-service nrepl-client-atom model)]
        (swap! nrepl-client-atom assoc ::ai-service ai)
        ai)))

(defn architect
  "Dispatches an architect agent with the given prompt and optional context.
   The agent will only have access to read tools.
   Returns a string response from the agent.
   
   Args:
   - tool-map: Map containing :nrepl-client-atom and optional :model
   - inputs: Map containing :prompt and optional :context"
  [{:keys [nrepl-client-atom model]} {:keys [prompt context]}]
  (if (string/blank? prompt)
    {:result "Error: Cannot process empty prompt"
     :error true}
    (if-let [ai-service (get-ai-service nrepl-client-atom model)]
      (do
        (.clear (:memory ai-service))
        (let [result (.chat (:service ai-service)
                            (cond-> prompt
                              (not (string/blank? context))
                              (str prompt "\n\n```context\n" context "\n```\n")))]
          {:result result
           :error false}))
      {:result "ERROR: No model configured for this agent."
       :error true})))

(defn validate-architect-inputs
  "Validates inputs for the architect function"
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

    (and (:context inputs) (not (string? (:context inputs))))
    (throw (ex-info "Parameter 'context' must be a string when provided"
                    {:error-details [(str "Got: " (type (:context inputs)))]}))

    :else
    inputs))

(def system-message
  "You are an expert Clojure software architect. Your role is to analyze technical requirements and produce clear, actionable implementation plans following Clojure idioms and functional programming principles.
These plans will then be carried out by a junior Clojure developer, so you need to be specific and detailed. However, do not actually write the code, just explain the plan.

Follow these steps for each request:
1. Carefully analyze requirements to identify core functionality and constraints
2. Define clear technical approach with specific Clojure libraries, functions, and patterns
3. Break down implementation into concrete, actionable steps at the appropriate level of abstraction

CLOJURE BEST PRACTICES TO FOLLOW:
- Emphasize functional programming with pure functions and immutable data structures
- Prefer proper conditionals: use 'if' for binary choices, 'cond' for multiple conditions, and 'if-let'/'when-let' for binding and testing in one step
- Recommend threading macros (-> and ->>) to eliminate intermediate bindings and improve readability
- Suggest destructuring in function parameters for cleaner access to data structures
- Design functions to do one thing well and return useful values
- Use early returns with 'when' rather than deeply nested conditionals
- Track actual values instead of boolean flags where possible
- Emphasize REPL-driven development with small, incrementally tested steps
- Organize code with thoughtful namespace design and clear dependency management
- Use appropriate Clojure abstractions like multimethods, protocols, or spec where relevant

Keep responses focused, specific and actionable. 

IMPORTANT: Do not ask the user if you should implement the changes at the end. Just provide the plan as described above.
IMPORTANT: Do not attempt to write the code or use any string modification tools. Just provide the plan.")
