(ns clojure-mcp.tools.project.core-test
  (:require
   [clojure-mcp.tools.project.core
    :refer [determine-project-type
            extract-bb-source-paths
            extract-bb-tasks
            extract-lein-project-info
            extract-source-paths
            extract-test-paths
            parse-lein-config
            read-bb-edn
            to-relative-path]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]])
  (:import (java.nio.file Files)))

(deftest lein-project-parsing-test
  (testing "parses real Leiningen project.clj file"
    (let [project-file (io/resource "clojure-mcp/test/projects/project.clj")
          project-content (-> project-file slurp read-string)
          lein-config (->> project-content
                           (drop 3)
                           (partition 2)
                           (map (fn [[k v]] [k v]))
                           (into {}))
          project-name (second project-content)
          version (nth project-content 2)]

      (is (= 'acme/widget-factory project-name))
      (is (= "2.3.0-SNAPSHOT" version))
      (is (= "A comprehensive widget manufacturing system" (:description lein-config)))
      (is (= "https://github.com/acme/widget-factory" (:url lein-config)))
      (is (map? (:license lein-config)))
      (is (= "Eclipse Public License" (get-in lein-config [:license :name])))
      (is (vector? (:dependencies lein-config)))
      (is (= 3 (count (:dependencies lein-config))))
      (is (= ["src/main/clj" "src/shared"] (:source-paths lein-config)))
      (is (= ["test/unit" "test/integration"] (:test-paths lein-config)))
      (is (= {:port 57802} (:repl-options lein-config)))
      (is (map? (:profiles lein-config))))))

(deftest lein-project-defaults-test
  (testing "handles default source and test paths for Leiningen projects"
    (let [minimal-project '(defproject my-app "1.0.0"
                             :dependencies [['org.clojure/clojure "1.11.1"]])

          lein-config (->> minimal-project
                           (drop 3)
                           (partition 2)
                           (map (fn [[k v]] [k v]))
                           (into {}))

          ;; Test default path logic
          source-paths (or (:source-paths lein-config) ["src"])
          test-paths (or (:test-paths lein-config) ["test"])]

      (is (= ["src"] source-paths))
      (is (= ["test"] test-paths))))

  (testing "respects explicit source and test paths"
    (let [custom-paths-project '(defproject my-app "1.0.0"
                                  :source-paths ["src/main/clj" "src/shared"]
                                  :test-paths ["test/unit" "test/integration"]
                                  :dependencies [['org.clojure/clojure "1.11.1"]])

          lein-config (->> custom-paths-project
                           (drop 3)
                           (partition 2)
                           (map (fn [[k v]] [k v]))
                           (into {}))

          source-paths (or (:source-paths lein-config) ["src"])
          test-paths (or (:test-paths lein-config) ["test"])]

      (is (= ["src/main/clj" "src/shared"] source-paths))
      (is (= ["test/unit" "test/integration"] test-paths)))))

(deftest deps-project-parsing-test
  (testing "parses real deps.edn file"
    (let [deps-file (io/resource "clojure-mcp/test/projects/deps.edn")
          deps-content (-> deps-file slurp edn/read-string)]

      (is (map? deps-content))
      (is (map? (:deps deps-content)))
      (is (= 3 (count (:deps deps-content))))
      (is (= ["src" "resources"] (:paths deps-content)))
      (is (map? (:aliases deps-content)))
      (is (contains? (:aliases deps-content) :test))
      (is (contains? (:aliases deps-content) :dev))
      (is (= ["test"] (get-in deps-content [:aliases :test :extra-paths])))
      (is (= ["dev"] (get-in deps-content [:aliases :dev :extra-paths]))))))

(deftest parse-lein-config-test
  (testing "parses valid project.clj configuration"
    (let [project-clj '(defproject my-app "1.0.0"
                         :description "Test app"
                         :dependencies [[org.clojure/clojure "1.11.1"]]
                         :source-paths ["src"]
                         :test-paths ["test"])
          config (parse-lein-config project-clj)]
      (is (= "Test app" (:description config)))
      (is (= 'org.clojure/clojure (first (first (:dependencies config)))))
      (is (= "1.11.1" (second (first (:dependencies config)))))
      (is (= ["src"] (:source-paths config)))
      (is (= ["test"] (:test-paths config)))))

  (testing "handles malformed project.clj"
    (is (nil? (parse-lein-config nil)))
    (is (nil? (parse-lein-config [])))
    (is (nil? (parse-lein-config '(defproject))))
    (is (nil? (parse-lein-config '(defproject my-app)))))

  (testing "handles odd number of config pairs"
    (let [project-clj '(defproject my-app "1.0.0" :description)
          config (parse-lein-config project-clj)]
      (is (nil? config)))))

(deftest extract-lein-project-info-test
  (testing "extracts basic project information"
    (let [project-clj '(defproject my-company/my-app "2.1.0"
                         :description "A great app"
                         :dependencies [[org.clojure/clojure "1.11.1"]]
                         :profiles {:dev {:dependencies [[midje "1.10.9"]]}})
          config (parse-lein-config project-clj)
          info (extract-lein-project-info project-clj config)]
      (is (= 'my-company/my-app (:name info)))
      (is (= "2.1.0" (:version info)))
      (is (= '[[org.clojure/clojure "1.11.1"]] (:dependencies info)))
      (is (= {:dev {:dependencies '[[midje "1.10.9"]]}} (:profiles info)))))

  (testing "handles minimal project.clj"
    (let [project-clj '(defproject basic "1.0.0")
          config (parse-lein-config project-clj)
          info (extract-lein-project-info project-clj config)]
      (is (= 'basic (:name info)))
      (is (= "1.0.0" (:version info)))
      (is (= [] (:dependencies info)))
      (is (= {} (:profiles info)))))

  (testing "handles malformed input"
    (let [info (extract-lein-project-info nil {})]
      (is (nil? info)))))

(deftest extract-source-paths-test
  (testing "extracts paths from bb.edn (highest priority)"
    (let [bb-config {:paths ["bb-src" "bb-resources"]}
          deps {:paths ["src" "resources"]}
          lein-config {:source-paths ["src/main/clj"]}]
      (is (= ["bb-src" "bb-resources"] (extract-source-paths deps lein-config bb-config)))))

  (testing "extracts paths from deps.edn when no bb.edn"
    (let [deps {:paths ["src" "resources"]}]
      (is (= ["src" "resources"] (extract-source-paths deps nil nil)))))

  (testing "extracts paths from lein config when no deps.edn or bb.edn"
    (let [lein-config {:source-paths ["src/main/clj" "src/shared"]}]
      (is (= ["src/main/clj" "src/shared"] (extract-source-paths nil lein-config nil)))))

  (testing "defaults to src when no paths specified"
    (is (= ["src"] (extract-source-paths nil {} nil)))
    (is (= ["src"] (extract-source-paths {} nil nil)))
    (is (= ["src"] (extract-source-paths nil nil nil))))

  (testing "handles invalid path configurations"
    (let [bad-deps {:paths "not-a-vector"}
          bad-lein {:source-paths 123}
          bad-bb {:paths 456}]
      (is (= ["src"] (extract-source-paths bad-deps nil nil)))
      (is (= ["src"] (extract-source-paths nil bad-lein nil)))
      (is (= ["src"] (extract-source-paths nil nil bad-bb))))))

(deftest extract-test-paths-test
  (testing "extracts test paths from deps.edn aliases"
    (let [deps {:aliases {:test {:extra-paths ["test" "test-integration"]}}}]
      (is (= ["test" "test-integration"] (extract-test-paths deps nil nil)))))

  (testing "extracts test paths from lein config"
    (let [lein-config {:test-paths ["test/unit" "test/integration"]}]
      (is (= ["test/unit" "test/integration"] (extract-test-paths nil lein-config nil)))))

  (testing "defaults to test when no paths specified"
    (is (= ["test"] (extract-test-paths nil {} nil)))
    (is (= ["test"] (extract-test-paths {} nil nil)))
    (is (= ["test"] (extract-test-paths nil nil nil))))

  (testing "handles invalid test path configurations"
    (let [bad-deps {:aliases {:test {:extra-paths "not-a-vector"}}}
          bad-lein {:test-paths 123}]
      (is (= ["test"] (extract-test-paths bad-deps nil nil)))
      (is (= ["test"] (extract-test-paths nil bad-lein nil))))))

(deftest determine-project-type-test
  (testing "identifies project types correctly"
    (is (= "deps.edn" (determine-project-type {:paths ["src"]} nil nil)))
    (is (= "Leiningen" (determine-project-type nil '(defproject app "1.0.0") nil)))
    (is (= "Babashka" (determine-project-type nil nil {:paths ["src"]})))
    (is (= "deps.edn + Leiningen" (determine-project-type {:paths ["src"]} '(defproject app "1.0.0") nil)))
    (is (= "deps.edn + Babashka" (determine-project-type {:paths ["src"]} nil {:paths ["src"]})))
    (is (= "Leiningen + Babashka" (determine-project-type nil '(defproject app "1.0.0") {:paths ["src"]})))
    (is (= "deps.edn + Leiningen + Babashka" (determine-project-type {:paths ["src"]} '(defproject app "1.0.0") {:paths ["src"]})))
    (is (= "Unknown" (determine-project-type nil nil nil)))))

(deftest bb-edn-parsing-test
  (testing "parses bb.edn file correctly"
    (let [temp-dir (Files/createTempDirectory "bb-test" (make-array java.nio.file.attribute.FileAttribute 0))
          bb-file (io/file (.toFile temp-dir) "bb.edn")]
      (try
        ;; Create a test bb.edn file
        (spit bb-file "{:paths [\"src\" \"resources\"] :deps {org.clojure/clojure {:mvn/version \"1.11.1\"}}}")
        (let [result (read-bb-edn (.toString temp-dir))]
          (is (map? result))
          (is (= ["src" "resources"] (:paths result)))
          (is (map? (:deps result))))
        (finally
          ;; Clean up
          (io/delete-file bb-file true)
          (Files/delete temp-dir)))))

  (testing "returns nil when bb.edn doesn't exist"
    (let [temp-dir (Files/createTempDirectory "bb-test-empty" (make-array java.nio.file.attribute.FileAttribute 0))]
      (try
        (is (nil? (read-bb-edn (.toString temp-dir))))
        (finally
          (Files/delete temp-dir)))))

  (testing "returns nil when bb.edn is malformed"
    (let [temp-dir (Files/createTempDirectory "bb-test-bad" (make-array java.nio.file.attribute.FileAttribute 0))
          bb-file (io/file (.toFile temp-dir) "bb.edn")]
      (try
        ;; Create a malformed bb.edn file
        (spit bb-file "{:paths [\"src\" :deps}")
        (is (nil? (read-bb-edn (.toString temp-dir))))
        (finally
          ;; Clean up
          (io/delete-file bb-file true)
          (Files/delete temp-dir))))))

(deftest extract-bb-source-paths-test
  (testing "extracts paths from bb config"
    (let [bb-config {:paths ["bb-src" "bb-resources"]}]
      (is (= ["bb-src" "bb-resources"] (extract-bb-source-paths bb-config)))))

  (testing "returns empty vector when no paths"
    (let [bb-config {:deps {}}]
      (is (= [] (extract-bb-source-paths bb-config)))))

  (testing "returns empty vector when bb-config is nil"
    (is (= [] (extract-bb-source-paths nil)))))

(deftest extract-bb-tasks-test
  (testing "extracts tasks from bb config"
    (let [bb-config {:tasks {:test {:doc "Run tests" :task '(println "testing")}
                             :build {:doc "Build project" :task '(println "building")}}}]
      (is (map? (extract-bb-tasks bb-config)))
      (is (contains? (extract-bb-tasks bb-config) :test))
      (is (contains? (extract-bb-tasks bb-config) :build))))

  (testing "returns nil when no tasks"
    (let [bb-config {:paths ["src"]}]
      (is (nil? (extract-bb-tasks bb-config)))))

  (testing "returns nil when bb-config is nil"
    (is (nil? (extract-bb-tasks nil)))))

(deftest to-relative-path-test
  (testing "converts absolute paths to relative"
    (let [working-dir "/home/user/project"
          file-path "/home/user/project/src/main.clj"]
      (is (= "src/main.clj" (to-relative-path working-dir file-path)))))

  (testing "handles same directory"
    (let [working-dir "/home/user/project"
          file-path "/home/user/project"]
      (is (= "" (to-relative-path working-dir file-path)))))

  (testing "returns original path when relativization fails"
    (let [working-dir "/home/user/project"
          file-path "/completely/different/path/file.clj"]
      (is (string? (to-relative-path working-dir file-path))))))
