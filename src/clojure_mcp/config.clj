(ns clojure-mcp.config
  (:require
   [clojure-mcp.dialects :as dialects]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]))

(defn- relative-to [dir path]
  (try
    (let [f (io/file path)]
      (if (.isAbsolute f)
        (.getCanonicalPath f)
        (.getCanonicalPath (io/file dir path))))
    (catch Exception e
      (log/warn e "Bad file paths " (pr-str [dir path]))
      nil)))

(defn process-config
  [{:keys [allowed-directories emacs-notify write-file-guard cljfmt
           bash-over-nrepl nrepl-env-type]
    :as config} user-dir]
  (let [ud (io/file user-dir)]
    (assert (and (.isAbsolute ud) (.isDirectory ud)))
    (when (some? write-file-guard)
      (when-not (contains? #{:full-read :partial-read false} write-file-guard)
        (log/warn "Invalid write-file-guard value:" write-file-guard
                  "- using default :full-read")
        (throw (ex-info (str "Invalid Config: write-file-guard value:  " write-file-guard
                             "- must be one of (:full-read, :partial-read, false)")
                        {:write-file-guard write-file-guard}))))
    (cond-> config
      :always
      (assoc :allowed-directories
             (->> (cons user-dir allowed-directories)
                  (keep #(relative-to user-dir %))
                  distinct
                  vec))

      (some? user-dir)
      (assoc :nrepl-user-dir (.getCanonicalPath ud))

      (some? emacs-notify)
      (assoc :emacs-notify (boolean emacs-notify))

      (some? cljfmt)
      (assoc :cljfmt (boolean cljfmt))

      (some? bash-over-nrepl)
      (assoc :bash-over-nrepl (boolean bash-over-nrepl))

      (some? nrepl-env-type)
      (assoc :nrepl-env-type nrepl-env-type))))

(defn load-config
  "Loads configuration from .clojure-mcp/config.edn in the given directory.
   Reads the file directly from the filesystem."
  [cli-config-file user-dir]
  (let [config-file (if cli-config-file
                      (io/file cli-config-file)
                      (io/file user-dir ".clojure-mcp" "config.edn"))
        config (if (.exists config-file)
                 (try
                   (edn/read-string (slurp config-file))
                   (catch Exception e
                     (log/warn e "Failed to read config file:" (.getPath config-file))
                     {}))
                 {})
        processed-config (process-config config user-dir)]
    (log/info "Config file:" (.getPath config-file) "exists:" (.exists config-file))
    (log/info "Raw config:" config)
    (log/info "Processed config:" processed-config)
    processed-config))

(defn get-config [nrepl-client-map k]
  (get-in nrepl-client-map [::config k]))

(defn get-allowed-directories [nrepl-client-map]
  (get-config nrepl-client-map :allowed-directories))

(defn get-emacs-notify [nrepl-client-map]
  (get-config nrepl-client-map :emacs-notify))

(defn get-nrepl-user-dir [nrepl-client-map]
  (get-config nrepl-client-map :nrepl-user-dir))

(defn get-cljfmt [nrepl-client-map]
  (let [value (get-config nrepl-client-map :cljfmt)]
    (if (nil? value)
      true ; Default to true when not specified
      value)))

(defn get-write-file-guard [nrepl-client-map]
  (let [value (get-config nrepl-client-map :write-file-guard)]
    ;; Validate the value and default to :full-read if invalid
    (cond
      ;; nil means not configured, use default
      (nil? value) :full-read
      ;; Valid values (including false)
      (contains? #{:full-read :partial-read false} value) value
      ;; Invalid values
      :else (do
              (log/warn "Invalid write-file-guard value:" value "- using default :full-read")
              :full-read))))

(defn get-nrepl-env-type
  "Returns the nREPL environment type.
   Defaults to :clj if not specified."
  [nrepl-client-map]
  (let [value (get-config nrepl-client-map :nrepl-env-type)]
    (if (nil? value)
      :clj ; Default to :clj when not specified
      value)))

(defn get-bash-over-nrepl
  "Returns whether bash commands should be executed over nREPL.
   Defaults to true for compatibility."
  [nrepl-client-map]
  (let [value (get-config nrepl-client-map :bash-over-nrepl)
        nrepl-env-type (get-nrepl-env-type nrepl-client-map)]
    ;; XXX hack so that bash still works in other environments
    (if (nil? value)
      ;; default to the capability
      (dialects/handle-bash-over-nrepl? nrepl-env-type)
      ;; respect configured value
      (boolean value))))

(defn clojure-env?
  "Returns true if the nREPL environment is a Clojure environment."
  [nrepl-client-map]
  (= :clj (get-nrepl-env-type nrepl-client-map)))

(defn write-guard?
  "Returns true if write-file-guard is enabled (not false).
   This means file timestamp checking is active."
  [nrepl-client-map]
  (not= false (get-write-file-guard nrepl-client-map)))

(defn get-scratch-pad-load
  "Returns whether scratch pad persistence is enabled.
   Defaults to false when not specified."
  [nrepl-client-map]
  (let [value (get-config nrepl-client-map :scratch-pad-load)]
    (if (nil? value)
      false ; Default to false when not specified
      (boolean value))))

(defn get-scratch-pad-file
  "Returns the scratch pad filename.
   Defaults to 'scratch_pad.edn' when not specified."
  [nrepl-client-map]
  (let [value (get-config nrepl-client-map :scratch-pad-file)]
    (if (nil? value)
      "scratch_pad.edn" ; Default filename
      value)))

(defn get-mcp-client-hint [nrepl-client-map]
  (get-config nrepl-client-map :mcp-client-hint))

(defn get-dispatch-agent-context
  "Returns dispatch agent context configuration.
   Can be:
   - true/false (boolean) - whether to use default code index
   - list of file paths - specific files to load into context
   Defaults to true for backward compatibility."
  [nrepl-client-map]
  (let [value (get-config nrepl-client-map :dispatch-agent-context)
        user-dir (get-nrepl-user-dir nrepl-client-map)]
    (cond
      (nil? value)
      true ;; Default to true to maintain existing behavior

      (boolean? value)
      value

      (sequential? value)
      ;; Process file paths
      (->> value
           (map (fn [path]
                  (let [file (io/file path)]
                    (if (.isAbsolute file)
                      (.getCanonicalPath file)
                      (.getCanonicalPath (io/file user-dir path))))))
           (filter #(.exists (io/file %)))
           vec)

      :else
      (do
        (log/warn "Invalid :dispatch-agent-context value, defaulting to true")
        true))))

(defn set-config*
  "Sets a config value in a map. Returns the updated map.
   This is the core function that set-config! uses."
  [nrepl-client-map k v]
  (assoc-in nrepl-client-map [::config k] v))

(defn set-config!
  "Sets a config value in an atom containing an nrepl-client map.
   Uses set-config* to perform the actual update."
  [nrepl-client-atom k v]
  (swap! nrepl-client-atom set-config* k v))
