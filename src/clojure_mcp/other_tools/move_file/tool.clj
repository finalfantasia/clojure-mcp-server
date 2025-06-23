(ns clojure-mcp.other-tools.move-file.tool
  "Implementation of the move-file tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.other-tools.move-file.core :as core]
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.utils.valid-paths :as valid-paths]))

;; Factory function to create the tool configuration
(defn create-move-file-tool
  "Creates the move-file tool configuration"
  [nrepl-client-atom]
  {:tool-type :move-file
   :nrepl-client-atom nrepl-client-atom})

;; Implement the required multimethods for the move-file tool
(defmethod tool-system/tool-name :move-file [_]
  "move_file")

(defmethod tool-system/tool-description :move-file [_]
  "Move or rename files and directories. Can move files between directories and rename them in a single operation. If the destination exists, the operation will fail. Works across different directories and can be used for simple renaming within the same directory. Both source and destination must be within allowed directories.")

(defmethod tool-system/tool-schema :move-file [_]
  {:type :object
   :properties {:source {:type :string
                         :description "The source file or directory path to move or rename."}
                :destination {:type :string
                              :description "The destination file or directory path."}}
   :required [:source :destination]})

(defmethod tool-system/validate-inputs :move-file [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [source destination]} inputs
        nrepl-client @nrepl-client-atom]
    ;; Validate required parameters
    (when-not source
      (throw (ex-info "Missing required parameter: source" {:inputs inputs})))

    (when-not destination
      (throw (ex-info "Missing required parameter: destination" {:inputs inputs})))

    ;; Validate both paths using the utility function
    (let [validated-source (valid-paths/validate-path-with-client source nrepl-client)
          validated-destination (valid-paths/validate-path-with-client destination nrepl-client)]
      ;; Return validated inputs with normalized paths
      (assoc inputs
             :source validated-source
             :destination validated-destination))))

(defmethod tool-system/execute-tool :move-file [_ inputs]
  (let [{:keys [source destination]} inputs]
    ;; Delegate to core implementation
    (core/move-file source destination)))

(defmethod tool-system/format-results :move-file
  [_ {:keys [success error source destination type] :as _result}]
  (if success
    ;; Success case
    {:result [(str "Successfully moved " type " from " source " to " destination)]
     :error false}
    ;; Error case
    {:result [(or error "Unknown error during move operation")]
     :error true}))

;; Backward compatibility function that returns the registration map
(defn move-file-tool [nrepl-client-atom]
  (tool-system/registration-map (create-move-file-tool nrepl-client-atom)))

(comment
  ;; === Examples of using the move-file tool ===
  (require 'clojure-mcp.nrepl)
  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)

  ;; Create a tool instance
  (def move-tool (create-move-file-tool client-atom))

  ;; Test the individual multimethod steps
  (def inputs {:source "/tmp/test-source.txt" :destination "/tmp/test-dest.txt"})
  (def validated (tool-system/validate-inputs move-tool inputs))
  (def result (tool-system/execute-tool move-tool validated))
  (def formatted (tool-system/format-results move-tool result))

  ;; Generate the full registration map
  (def reg-map (tool-system/registration-map move-tool))

  ;; Test running the tool-fn directly
  (def tool-fn (:tool-fn reg-map))
  (tool-fn nil {"source" "/tmp/test-source.txt" "destination" "/tmp/test-dest.txt"}
           (fn [result error] (tap> (str "Result:" result "Error:" error))))

  ;; Clean up
  (clojure-mcp.nrepl/stop-polling @client-atom))
