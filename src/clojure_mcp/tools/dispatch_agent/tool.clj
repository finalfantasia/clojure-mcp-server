(ns clojure-mcp.tools.dispatch-agent.tool
  (:require [clojure-mcp.tool-system :as tool-system]
            [clojure-mcp.tools.dispatch-agent.core :as core]))

(defn create-dispatch-agent-tool
  "Creates the dispatch agent tool configuration.

   Args:
   - nrepl-client-atom: Required nREPL client atom
   - model: Optional pre-built langchain model to use instead of auto-detection"
  ([nrepl-client-atom]
   (create-dispatch-agent-tool nrepl-client-atom nil))
  ([nrepl-client-atom model]
   {:tool-type :dispatch-agent
    :nrepl-client-atom nrepl-client-atom
    :model model}))

(defn dispatch-agent-tool
  "Returns a tool registration for the dispatch-agent tool compatible with the MCP system.

   Usage:

   Basic usage with auto-detected model:
   (dispatch-agent-tool nrepl-client-atom)

   With custom model configuration:
   (dispatch-agent-tool nrepl-client-atom {:model my-custom-model})

   Where:
   - nrepl-client-atom: Required nREPL client atom
   - config: Optional config map with keys:
     - :model - Pre-built langchain model to use instead of auto-detection

   Examples:
   ;; Default model
   (def my-agent (dispatch-agent-tool nrepl-client-atom))

   ;; Custom Anthropic model
   (def fast-model (-> (chain/create-anthropic-model \"claude-3-haiku-20240307\") (.build)))
   (def fast-agent (dispatch-agent-tool nrepl-client-atom {:model fast-model}))

   ;; Custom OpenAI model
   (def reasoning-model (-> (chain/create-openai-model \"o1-preview\") (.build)))
   (def reasoning-agent (dispatch-agent-tool nrepl-client-atom {:model reasoning-model}))"
  ([nrepl-client-atom]
   (dispatch-agent-tool nrepl-client-atom nil))
  ([nrepl-client-atom {:keys [model]}]
   (tool-system/registration-map (create-dispatch-agent-tool nrepl-client-atom model))))

(defmethod tool-system/tool-name :dispatch-agent [_]
  "dispatch_agent")

(defmethod tool-system/tool-description :dispatch-agent [_]
  "Launch a new agent that has access to read-only tools. When you are searching for a keyword or file and are not confident that you will find the right match on the first try, use the Agent tool to perform the search for you. For example:

- If you are searching for a keyword like \"config\" or \"logger\", the Agent tool is appropriate
- If you want to read a specific file path, use the read_file or glob_files tool instead of the Agent tool, to find the match more quickly
- If you are searching for a specific class definition like \"class Foo\", use the glob_files tool instead, to find the match more quickly

Usage notes:
1. Launch multiple agents concurrently whenever possible, to maximize performance; to do that, use a single message with multiple `agent` tool uses
2. When the agent is done, it will return a single message back to you. The result returned by the agent is not visible to the user. To show the user the result, you should send a text message back to the user with a concise summary of the result.
3. Each agent invocation is stateless. You will not be able to send additional messages to the agent, nor will the agent be able to communicate with you outside its final report. Therefore, your prompt should contain a highly detailed task description for the agent to perform autonomously, and you should specify exactly what information the agent should return back to you in its final and only message to you.
4. The agent's outputs should generally be trusted")

(defmethod tool-system/tool-schema :dispatch-agent [_]
  {:type :object
   :properties {:prompt {:type :string
                         :description "The prompt to send to the agent"}}
   :required [:prompt]})

(defmethod tool-system/validate-inputs :dispatch-agent [_ inputs]
  (core/validate-dispatch-agent-inputs inputs))

(defmethod tool-system/execute-tool :dispatch-agent [tool {:keys [prompt]}]
  (core/dispatch-agent tool prompt))

(defmethod tool-system/format-results :dispatch-agent
  [_ {:keys [result error] :as _results}]
  {:result [result]
   :error error})
