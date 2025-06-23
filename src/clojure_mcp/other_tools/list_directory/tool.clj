(ns clojure-mcp.other-tools.list-directory.tool
  "Implementation of the list-directory tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.other-tools.list-directory.core :as core]
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.utils.valid-paths :as valid-paths]))

;; Factory function to create the tool configuration
(defn create-list-directory-tool
  "Creates the list-directory tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :list-directory
   :nrepl-client-atom nrepl-client-atom})

;; Helper function to format directory listing output
(defn format-directory-listing
  "Format directory listing into a readable string representation.
   
   Parameters:
   - result: Result map from list-directory function
   
   Returns a formatted string with directory content listing"
  [result]
  (if (:error result)
    (:error result)
    (let [{:keys [files directories full-path]} result
          sb (StringBuilder.)]
      (.append sb (str "Directory: " full-path "\n"))
      (.append sb "===============================\n")
      (when (seq directories)
        (.append sb "\nDirectories:\n")
        (doseq [dir (sort directories)]
          (.append sb (str "[DIR] " dir "\n"))))
      (when (seq files)
        (.append sb "\nFiles:\n")
        (doseq [file (sort files)]
          (.append sb (str "[FILE] " file "\n"))))
      (when (and (empty? directories) (empty? files))
        (.append sb "Directory is empty\n"))
      (.toString sb))))

;; Implement the required multimethods for the list-directory tool
(defmethod tool-system/tool-name :list-directory [_]
  "fs_list_directory")

(defmethod tool-system/tool-description :list-directory [_]
  "Lists all files and directories at the specified path. 
Returns a formatted directory listing with files and subdirectories clearly labeled.")

(defmethod tool-system/tool-schema :list-directory [_]
  {:type :object
   :properties {:path {:type :string}}
   :required [:path]})

(defmethod tool-system/validate-inputs :list-directory [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))

    ;; Use the existing validate-path-with-client function
    (let [validated-path (valid-paths/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      {:path validated-path})))

(defmethod tool-system/execute-tool :list-directory [_ inputs]
  (let [{:keys [path]} inputs]
    ;; Call our implementation in core namespace
    (core/list-directory path)))

(defmethod tool-system/format-results :list-directory [_ result]
  (if (and (map? result) (:error result))
    ;; If there's an error, return it with error flag true
    {:result [(:error result)]
     :error true}
    ;; Otherwise, format the directory listing and return it
    {:result [(format-directory-listing result)]
     :error false}))

;; Backward compatibility function that returns the registration map
(defn list-directory-tool
  "Returns the registration map for the list-directory tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-list-directory-tool nrepl-client-atom)))