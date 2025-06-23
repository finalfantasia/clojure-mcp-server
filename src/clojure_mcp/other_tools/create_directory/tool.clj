(ns clojure-mcp.other-tools.create-directory.tool
  "Implementation of the create-directory tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.other-tools.create-directory.core :as core]
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.utils.valid-paths :as valid-paths]))

;; Factory function to create the tool configuration
(defn create-directory-tool
  "Creates the create-directory tool configuration"
  [nrepl-client-atom]
  {:tool-type :create-directory
   :nrepl-client-atom nrepl-client-atom})

;; Implement the required multimethods for the create-directory tool
(defmethod tool-system/tool-name :create-directory [_]
  "create_directory")

(defmethod tool-system/tool-description :create-directory [_]
  "Create a new directory or ensure a directory exists. Can create multiple nested directories in one operation. If the directory already exists, this operation will succeed silently. Perfect for setting up directory structures for projects or ensuring required paths exist. Only works within allowed directories.")

(defmethod tool-system/tool-schema :create-directory [_]
  {:type :object
   :properties {:path {:type :string
                       :description "The path to the directory to create or ensure exists."}}
   :required [:path]})

(defmethod tool-system/validate-inputs :create-directory [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path]} inputs
        nrepl-client @nrepl-client-atom]
    ;; Validate required parameters
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))

    ;; Validate path using the utility function
    (let [validated-path (valid-paths/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      (assoc inputs :path validated-path))))

(defmethod tool-system/execute-tool :create-directory [_ inputs]
  (let [{:keys [path]} inputs]
    ;; Delegate to core implementation
    (core/create-directory path)))

(defmethod tool-system/format-results :create-directory
  [_ {:keys [success error path exists created] :as _result}]
  (if success
    ;; Success case
    {:result [(cond
                exists (str "Directory already exists: " path)
                created (str "Created directory: " path)
                :else (str "Directory operation completed: " path))]
     :error false}
    ;; Error case
    {:result [(or error "Unknown error creating directory")]
     :error true}))

;; Backward compatibility function that returns the registration map
(defn create-directory-tool-registration [nrepl-client-atom]
  (tool-system/registration-map (create-directory-tool nrepl-client-atom)))

(comment
  ;; === Examples of using the create-directory tool ===
  (require 'clojure-mcp.nrepl)
  ;; Setup for REPL-based testing
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  (clojure-mcp.nrepl/start-polling @client-atom)

  ;; Create a tool instance
  (def dir-tool (create-directory-tool client-atom))

  ;; Test the individual multimethod steps
  (def inputs {:path "/tmp/test-dir/nested"})
  (def validated (tool-system/validate-inputs dir-tool inputs))
  (def result (tool-system/execute-tool dir-tool validated))
  (def formatted (tool-system/format-results dir-tool result))

  ;; Generate the full registration map
  (def reg-map (tool-system/registration-map dir-tool))

  ;; Test running the tool-fn directly
  (def tool-fn (:tool-fn reg-map))
  (tool-fn nil {"path" "/tmp/test-dir/nested"}
           (fn [result error] (println "Result:" result "Error:" error)))

  ;; Clean up
  (clojure-mcp.nrepl/stop-polling @client-atom))
