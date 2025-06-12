(ns clojure-mcp.tools.project.core
  "Core functionality for project inspection and analysis.
   This namespace provides the implementation details for analyzing project structure."
  (:require
   [clojure-mcp.config :as config]
   [clojure-mcp.nrepl :as nrepl]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn inspect-project-code
  "REPL expression to gather project information including paths, dependencies, and source files.
   Returns a map with project details that can be evaluated in the project's context."
  []
  '(let [deps (when (.exists (clojure.java.io/file "deps.edn"))
                (-> "deps.edn" slurp clojure.edn/read-string))
         project-clj (when (.exists (clojure.java.io/file "project.clj"))
                       (-> "project.clj" slurp read-string))
         lein-project? (boolean project-clj)
         deps-project? (boolean deps)
         ;; Parse Leiningen project.clj structure
         lein-config (when lein-project?
                       (->> project-clj
                            (drop 3) ; skip defproject, name, version
                            (partition 2)
                            (map (fn [[k v]] [k v]))
                            (into {})))
         source-files (fn [dir]
                        (when (.exists (clojure.java.io/file dir))
                          (->> (clojure.java.io/file dir)
                               file-seq
                               (filter #(.isFile %))
                               (filter #(let [name (.getName %)]
                                          (or (.endsWith name ".clj")
                                              (.endsWith name ".cljs")
                                              (.endsWith name ".cljc")
                                              (.endsWith name ".bb")
                                              (.endsWith name ".edn"))))
                               (map #(.getPath %))
                               sort)))
         ;; Extract paths from deps.edn or project.clj
         source-paths (cond
                        deps (or (:paths deps) ["src"])
                        lein-project? (or (:source-paths lein-config) ["src"])
                        :else ["src"])
         test-paths (cond
                      deps (get-in deps [:aliases :test :extra-paths] ["test"])
                      lein-project? (or (:test-paths lein-config) ["test"])
                      :else ["test"])
         all-paths (concat source-paths test-paths)
         sources (mapcat source-files all-paths)
         java-version (System/getProperty "java.version")
         clojure-version-info (clojure-version)]
     {:working-dir (System/getProperty "user.dir")
      :project-type (cond
                      (and deps-project? lein-project?) "deps.edn + Leiningen"
                      deps-project? "deps.edn"
                      lein-project? "Leiningen"
                      :else "Unknown")
      :java-version java-version
      :clj-version clojure-version-info
      :deps deps
      :project-clj (when lein-project?
                     {:name (nth project-clj 1)
                      :version (nth project-clj 2)
                      :dependencies (:dependencies lein-config)
                      :profiles (:profiles lein-config)})
      :source-paths source-paths
      :test-paths test-paths
      :sources sources}))

(defn format-project-info
  "Formats the project information into a readable string.

   Arguments:
   - insp-data: The project inspection data as an EDN string
   - allowed-directories: Optional list of allowed directories

   Returns a formatted string with project details"
  [insp-data & [allowed-directories]]
  (when insp-data
    (when-let [{:keys [working-dir
                       project-type
                       clj-version
                       java-version
                       deps
                       project-clj
                       source-paths
                       test-paths
                       sources]}
               (try
                 (edn/read-string insp-data)
                 (catch Throwable e
                   (println "Error parsing data:" (ex-message e))
                   nil))]
      (with-out-str
        (println "\nClojure Project Information:")
        (println "==============================")

        (println "\nEnvironment:")
        (println "• Working Directory:" working-dir)
        (println "• Project Type:" project-type)
        (println "• Clojure Version:" clj-version)
        (println "• Java Version:" java-version)

        (println "\nSource Paths:")
        (doseq [path source-paths]
          (println "•" path))

        (println "\nTest Paths:")
        (doseq [path test-paths]
          (println "•" path))

        (when allowed-directories
          (println "\nOther Relevant Accessible Directories:")
          (doseq [dir allowed-directories]
            (println "•" dir)))

        (when deps
          (println "\nDependencies:")
          (doseq [[dep coord] (sort-by key (:deps deps))]
            (println "•" dep "=>" coord)))

        (when-let [aliases (:aliases deps)]
          (println "\nAliases:")
          (doseq [[alias config] (sort-by key aliases)]
            (println "•" alias ":" (pr-str config))))

        (when project-clj
          (println "\nLeiningen Project:")
          (println "• Name:" (:name project-clj))
          (println "• Version:" (:version project-clj))
          (when-let [deps (:dependencies project-clj)]
            (println "\nLeiningen Dependencies:")
            (doseq [[dep version] deps]
              (println "•" dep "=>" version)))
          (when-let [profiles (:profiles project-clj)]
            (println "\nLeiningen Profiles:")
            (doseq [[profile config] (sort-by key profiles)]
              (println "•" profile ":" (pr-str config)))))

        (let [limit 50
              all-paths (concat source-paths test-paths)
              ;; Process raw file paths into proper namespace names
              processed-namespaces (->> sources
                                        (filter #(or (.endsWith % ".clj")
                                                     (.endsWith % ".cljs")
                                                     (.endsWith % ".cljc")))
                                        (map (fn [file-path]
                                               ;; Remove source path prefix from file path
                                               (let [relative-path (reduce (fn [path src-path]
                                                                             (if (.startsWith path (str src-path "/"))
                                                                               (.substring path (inc (count src-path)))
                                                                               path))
                                                                           file-path
                                                                           all-paths)]
                                                 (-> relative-path
                                                     (.replace "/" ".")
                                                     (.replace "_" "-")
                                                     (str/replace #"\.(clj|cljs|cljc)$" "")))))
                                        (into []))]
          (println "\nNamespaces (" (count processed-namespaces) "):")
          (doseq [ns-name (take limit processed-namespaces)]
            (println "•" ns-name))
          (when (> (count processed-namespaces) limit)
            (println "• ... and" (- (count processed-namespaces) limit) "more"))

          (println "\nProject Structure (" (count sources) " files):")
          (doseq [source-file (take limit sources)]
            (println "•" source-file))
          (when (> (count sources) limit)
            (println "• ... and" (- (count sources) limit) "more")))))))

(defn inspect-project
  "Core function to inspect a Clojure project and return formatted information.

   Arguments:
   - nrepl-client: The nREPL client connection

   Returns a map with :outputs (containing the formatted project info) and :error (boolean)"
  [nrepl-client]
  (let [insp-code (str (inspect-project-code))
        result-promise (promise)
        allowed-directories (config/get-allowed-directories nrepl-client)]
    (try
      (let [edn-result (nrepl/tool-eval-code nrepl-client insp-code)]
        (if (or (nil? edn-result) (.startsWith edn-result "Error"))
          (deliver result-promise
                   {:outputs [(or edn-result "Error during project inspection")]
                    :error true})
          (let [formatted-info (format-project-info edn-result allowed-directories)]
            (deliver result-promise
                     {:outputs [formatted-info]
                      :error false}))))
      (catch Exception e
        (deliver result-promise
                 {:outputs [(str "Exception during project inspection: " (.getMessage e))]
                  :error true})))
    @result-promise))

(comment
  ;; Test the project inspection in the REPL
  (def client (nrepl/create {:port 7888}))
  (nrepl/start-polling client)

  ;; Test inspection
  (def result (inspect-project client))
  (println (first (:outputs result)))

  ;; Clean up
  (nrepl/stop-polling client))
