(ns clojure-mcp.tools.project.core-test
  (:require [clojure-mcp.tools.project.core :refer [inspect-project-code]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(deftest inspect-project-code-test
  (testing "inspect-project-code generates valid REPL expression"
    (let [code (inspect-project-code)]
      (is (seq? code))
      (is (= 'let (first code))))))

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
