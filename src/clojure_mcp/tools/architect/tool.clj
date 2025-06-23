(ns clojure-mcp.tools.architect.tool
  (:require [clojure-mcp.tool-system :as tool-system]
            [clojure-mcp.tools.architect.core :as core]))

(defn create-architect-tool
  "Creates the architect tool configuration.
   
   Args:
   - nrepl-client-atom: Required nREPL client atom
   - model: Optional pre-built langchain model to use instead of auto-detection"
  ([nrepl-client-atom]
   (create-architect-tool nrepl-client-atom nil))
  ([nrepl-client-atom model]
   {:tool-type :architect
    :nrepl-client-atom nrepl-client-atom
    :model model}))

(defn architect-tool
  "Returns a tool registration for the architect tool compatible with the MCP system.
   
   Usage:
   
   Basic usage with auto-detected reasoning model:
   (architect-tool nrepl-client-atom)
   
   With custom model configuration:
   (architect-tool nrepl-client-atom {:model my-custom-model})
   
   Where:
   - nrepl-client-atom: Required nREPL client atom
   - config: Optional config map with keys:
     - :model - Pre-built langchain model to use instead of auto-detection
   
   Examples:
   ;; Default reasoning model (with medium reasoning effort)
   (def my-architect (architect-tool nrepl-client-atom))
   
   ;; Custom Anthropic model
   (def fast-model (-> (chain/create-anthropic-model \"claude-3-haiku-20240307\") (.build)))
   (def fast-architect (architect-tool nrepl-client-atom {:model fast-model}))
   
   ;; Custom OpenAI reasoning model with high effort
   (def reasoning-model (-> (chain/create-openai-model \"o1-preview\")
                            (chain/default-request-parameters #(chain/reasoning-effort % :high))
                            (.build)))
   (def reasoning-architect (architect-tool nrepl-client-atom {:model reasoning-model}))"
  ([nrepl-client-atom]
   (architect-tool nrepl-client-atom nil))
  ([nrepl-client-atom {:keys [model]}]
   (tool-system/registration-map (create-architect-tool nrepl-client-atom model))))

(defmethod tool-system/tool-name :architect [_]
  "architect")

(defmethod tool-system/tool-description :architect [_]
  "Your go-to tool for any technical or coding task. Analyzes requirements and breaks them down into clear, actionable implementation steps. Use this whenever you need help planning how to implement a feature, solve a technical problem, or structure your code.")

(defmethod tool-system/tool-schema :architect [_]
  {:type :object
   :properties {:prompt {:type :string
                         :description "The technical request or coding task to analyze"}
                :context {:type :string
                          :description "Optional context from previous conversation or system state"}}
   :required [:prompt]})

(defmethod tool-system/validate-inputs :architect [_ inputs]
  (core/validate-architect-inputs inputs))

(defmethod tool-system/execute-tool :architect [tool inputs]
  (core/architect tool inputs))

(defmethod tool-system/format-results :architect
  [_ {:keys [result error] :as _results}]
  {:result [result]
   :error error})
