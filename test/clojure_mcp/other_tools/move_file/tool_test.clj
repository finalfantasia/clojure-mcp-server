(ns clojure-mcp.other-tools.move-file.tool-test
  (:require [clojure-mcp.config :as config]
            [clojure-mcp.other-tools.move-file.tool :as tool]
            [clojure-mcp.tool-system :as tool-system]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; Create a mock nREPL client atom for testing
(def mock-client-atom (atom {}))
(config/set-config! mock-client-atom :nrepl-user-dir (System/getProperty "user.dir"))
(config/set-config! mock-client-atom :allowed-directories [(System/getProperty "user.dir")])

;; Test the tool-name multimethod
(deftest tool-name-test
  (testing "Tool name is correct"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)]
      (is (= "move_file" (tool-system/tool-name tool-config))))))

;; Test the tool-description multimethod
(deftest tool-description-test
  (testing "Tool description is not empty"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          description (tool-system/tool-description tool-config)]
      (is (string? description))
      (is (not (str/blank? description))))))

;; Test the tool-schema multimethod
(deftest tool-schema-test
  (testing "Schema includes required properties"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          schema (tool-system/tool-schema tool-config)]
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :source))
      (is (contains? (:properties schema) :destination))
      (is (= [:source :destination] (:required schema))))))

;; Test the validate-inputs multimethod
(deftest validate-inputs-test
  (testing "Validation rejects missing source parameter"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          inputs {:destination "/tmp/dest.txt"}]
      (is (thrown? Exception (tool-system/validate-inputs tool-config inputs)))))

  (testing "Validation rejects missing destination parameter"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          inputs {:source "/tmp/source.txt"}]
      (is (thrown? Exception (tool-system/validate-inputs tool-config inputs))))))

;; Test the format-results multimethod
(deftest format-results-test
  (testing "Format successful results"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          result {:success true
                  :source "/source.txt"
                  :destination "/dest.txt"
                  :type "file"}
          formatted (tool-system/format-results tool-config result)]
      (is (not (:error formatted)))
      (is (vector? (:result formatted)))
      (is (= 1 (count (:result formatted))))
      (is (str/includes? (first (:result formatted)) "Successfully moved"))))

  (testing "Format error results"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          result {:success false
                  :source "/source.txt"
                  :destination "/dest.txt"
                  :error "Test error message"}
          formatted (tool-system/format-results tool-config result)]
      (is (:error formatted))
      (is (vector? (:result formatted)))
      (is (= 1 (count (:result formatted))))
      (is (= "Test error message" (first (:result formatted)))))))

;; Test registration map creation
(deftest registration-map-test
  (testing "Creates a valid registration map"
    (let [tool-config (tool/create-move-file-tool mock-client-atom)
          reg-map (tool-system/registration-map tool-config)]
      (is (= "move_file" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (map? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))

;; Compatibility function test
(deftest compatibility-function-test
  (testing "move-file-tool function returns a valid registration map"
    (let [reg-map (tool/move-file-tool mock-client-atom)]
      (is (= "move_file" (:name reg-map)))
      (is (string? (:description reg-map)))
      (is (map? (:schema reg-map)))
      (is (fn? (:tool-fn reg-map))))))