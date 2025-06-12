(ns clojure-mcp.tools.read-file.tool
  "Implementation of the read-file tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.tools.read-file.file-timestamps :as file-timestamps]
   [clojure-mcp.utils.valid-paths :as valid-paths]
   [clojure.java.io :as io]))

;; Factory function to create the tool configuration
(defn create-read-file-tool
  "Creates the read-file tool configuration with optional parameters.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client
   - opts: Optional map with configuration options:
     - :max-lines: Maximum number of lines to read (default: 2000)
     - :max-line-length: Maximum length per line before truncation (default: 1000)"
  ([nrepl-client-atom]
   (create-read-file-tool nrepl-client-atom {}))
  ([nrepl-client-atom {:keys [max-lines max-line-length]
                       :or {max-lines 2000
                            max-line-length 1000}}]
   {:tool-type :read-file
    :nrepl-client-atom nrepl-client-atom
    :max-lines max-lines
    :max-line-length max-line-length}))

;; Implement the required multimethods for the read-file tool
(defmethod tool-system/tool-name :read-file [_]
  "fs_read_file")

(defmethod tool-system/tool-description :read-file [{:keys [max-lines max-line-length]}]
  (str "Reads the contents of a file. Requires absolute paths (not relative). "

       "IMPORTANT: For Clojure files, use clojure_file_structure first: "
       "- It's more token-efficient for exploring code structure "
       "- Only use fs_read_file when you need the complete file content or for non-Clojure files "

       "Example: To read a config file from line 10, limited to 50 lines: "
       "  {\"path\": \"/path/to/config.json\", \"line_offset\": 10, \"limit\": 50} "

       "Returns the file content wrapped in XML tags with metadata about truncation and size. "
       "By default, reads up to " max-lines " lines, truncating lines longer than " max-line-length " characters."))

(defmethod tool-system/tool-schema :read-file [_]
  {:type :object
   :properties {:path {:type :string
                       :description "The path to the file to read."}
                :line_offset {:type :integer
                              :description "Line number to start reading from (0-indexed)"}
                :limit {:type :integer
                        :description "Maximum number of lines to read"}}
   :required [:path]})

(defmethod tool-system/validate-inputs :read-file [{:keys [nrepl-client-atom]} inputs]
  (let [{:keys [path]} inputs
        nrepl-client @nrepl-client-atom]
    (when-not path
      (throw (ex-info "Missing required parameter: path" {:inputs inputs})))

    ;; Use the existing validate-path-with-client function
    (let [validated-path (valid-paths/validate-path-with-client path nrepl-client)]
      ;; Return validated inputs with normalized path
      (assoc inputs :path validated-path))))

(defmethod tool-system/execute-tool :read-file [{:keys [max-lines max-line-length nrepl-client-atom]} inputs]
  (let [{:keys [path line_offset limit]} inputs
        offset (or line_offset 0)
        limit (or limit max-lines)]
    (file-timestamps/read-file-with-timestamp
     nrepl-client-atom path offset limit :max-line-length max-line-length)))

(defmethod tool-system/format-results :read-file
  [_ {:keys [error path content truncated? line-count offset max-lines
             line-lengths-truncated? truncated-by] :as _result}]
  (if error
    ;; If there's an error, return it with error flag true
    {:result [error]
     :error true}
    ;; Otherwise, format the file content with metadata
    (let [file (io/file path)
          size (.length file)
          header (-> (str "<file-content path=\"" path "\" "
                          "byte-size=\"" size "\" "
                          "line-count=\"" line-count "\" "
                          "line-offset=\"" offset "\" "
                          "line-limit=\"" max-lines "\" "
                          "truncated=\"" (boolean truncated?) "\" ")
                     (cond->
                      truncated-by (str "truncated-by=\"" truncated-by "\" ")
                      line-lengths-truncated? (str "line-lengths-truncated=\"true\" "))
                     (str ">\n"))
          formatted (str header content "\n</file-content>")]
      {:result [formatted]
       :error false})))

;; Backward compatibility functions that return the registration map
(defn read-file-tool
  "Returns the registration map for the read-file tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client
   - opts: Optional map with configuration options:
     - :max-lines: Maximum number of lines to read (default: 2000)
     - :max-line-length: Maximum length per line before truncation (default: 1000)"
  ([nrepl-client-atom]
   (read-file-tool nrepl-client-atom {}))
  ([nrepl-client-atom opts]
   (tool-system/registration-map (create-read-file-tool nrepl-client-atom opts))))
