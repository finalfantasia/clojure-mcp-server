(ns clojure-mcp.other-tools.symbol.tool-test
  (:require
   [clojure-mcp.nrepl]
   [clojure-mcp.other-tools.symbol.core :as core]
   [clojure-mcp.other-tools.symbol.tool :as sut]
   [clojure-mcp.tool-system :as tool-system]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [nrepl.server]))

;; Use the common test utils for nREPL server setup
(defonce ^:dynamic *nrepl-server* nil)
(defonce ^:dynamic *nrepl-client-atom* nil)

(defn test-nrepl-fixture [f]
  (let [server (nrepl.server/start-server :port 0)
        port (:port server)
        client (clojure-mcp.nrepl/create {:port port})
        client-atom (atom client)]
    (clojure-mcp.nrepl/start-polling client)
    ;; Load some namespaces for testing
    (clojure-mcp.nrepl/eval-code client "(require 'clojure.repl)" identity)
    (clojure-mcp.nrepl/eval-code client "(require 'clojure.string)" identity)
    (binding [*nrepl-server* server
              *nrepl-client-atom* client-atom]
      (try
        (f)
        (finally
          (clojure-mcp.nrepl/stop-polling client)
          (nrepl.server/stop-server server))))))

(use-fixtures :once test-nrepl-fixture)

;; Mock core functions for reliable testing
(defn with-mocked-core-fns [f]
  (with-redefs [core/get-symbol-completions (fn [_ _] {:completions ["map" "mapv"] :error false})
                core/get-symbol-metadata (fn [_ _] {:metadata {:name 'map :doc "Doc" :arglists '([f coll])} :error false})
                core/get-symbol-documentation (fn [_ _] {:arglists '([f coll]) :doc "Doc" :error false})
                core/get-source-code (fn [_ _] {:source "(defn map [f coll] ...)" :error false})
                core/search-symbols (fn [_ _] {:matches ["clojure.core/map"] :error false})]
    (f)))

;; Helper to directly invoke the tool function from our tool registration map
(defn test-tool-execution [tool-name inputs]
  (with-mocked-core-fns
    (fn []
      (let [tool-instance (case tool-name
                            :symbol-completions (sut/create-symbol-completions-tool *nrepl-client-atom*)
                            :symbol-metadata (sut/create-symbol-metadata-tool *nrepl-client-atom*)
                            :symbol-documentation (sut/create-symbol-documentation-tool *nrepl-client-atom*)
                            :source-code (sut/create-source-code-tool *nrepl-client-atom*)
                            :symbol-search (sut/create-symbol-search-tool *nrepl-client-atom*))
            reg-map (tool-system/registration-map tool-instance)
            tool-fn (:tool-fn reg-map)
            promise-result (promise)]
        (tool-fn nil inputs
                 (fn [result error?]
                   (deliver promise-result {:result result :error? error?})))
        @promise-result))))

;; Helper to test just the individual steps of the pipeline
(defn test-pipeline-steps [tool-type inputs]
  (with-mocked-core-fns
    (fn []
      (let [tool-instance (case tool-type
                            :symbol-completions (sut/create-symbol-completions-tool *nrepl-client-atom*)
                            :symbol-metadata (sut/create-symbol-metadata-tool *nrepl-client-atom*)
                            :symbol-documentation (sut/create-symbol-documentation-tool *nrepl-client-atom*)
                            :source-code (sut/create-source-code-tool *nrepl-client-atom*)
                            :symbol-search (sut/create-symbol-search-tool *nrepl-client-atom*))
            validated (tool-system/validate-inputs tool-instance inputs)
            execution-result (tool-system/execute-tool tool-instance validated)
            formatted-result (tool-system/format-results tool-instance execution-result)]
        formatted-result))))

(deftest tool-name-test
  (testing "Tool names are correct"
    (is (= "symbol_completions"
           (tool-system/tool-name {:tool-type :symbol-completions})))
    (is (= "symbol_metadata"
           (tool-system/tool-name {:tool-type :symbol-metadata})))
    (is (= "symbol_documentation"
           (tool-system/tool-name {:tool-type :symbol-documentation})))
    (is (= "source_code"
           (tool-system/tool-name {:tool-type :source-code})))
    (is (= "symbol_search"
           (tool-system/tool-name {:tool-type :symbol-search})))))

(deftest tool-description-test
  (testing "Tool descriptions are non-empty"
    (doseq [tool-type [:symbol-completions :symbol-metadata :symbol-documentation
                       :source-code :symbol-search]]
      (let [description (tool-system/tool-description {:tool-type tool-type})]
        (is (string? description))
        (is (not (str/blank? description)))))))

(deftest tool-schema-test
  (testing "symbol-completions schema is correct"
    (let [schema (tool-system/tool-schema {:tool-type :symbol-completions})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :prefix))
      (is (= [:prefix] (:required schema)))))

  (testing "symbol-metadata schema is correct"
    (let [schema (tool-system/tool-schema {:tool-type :symbol-metadata})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :symbol))
      (is (= [:symbol] (:required schema)))))

  (testing "source-code schema is correct"
    (let [schema (tool-system/tool-schema {:tool-type :source-code})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :symbol))
      (is (= [:symbol] (:required schema))))))

(deftest validate-inputs-test
  (testing "symbol-completions validates prefix parameter"
    (let [completions-tool {:tool-type :symbol-completions}]
      (is (= {:prefix "map"}
             (tool-system/validate-inputs completions-tool {:prefix "map"})))
      (is (= {:prefix ""}
             (tool-system/validate-inputs completions-tool {:prefix ""})))
      (is (thrown? Exception (tool-system/validate-inputs completions-tool {})))))

  (testing "symbol-metadata validates symbol parameter"
    (let [metadata-tool {:tool-type :symbol-metadata}]
      (is (= {:symbol "map"}
             (tool-system/validate-inputs metadata-tool {:symbol "map"})))
      (is (thrown? Exception (tool-system/validate-inputs metadata-tool {})))))

  (testing "symbol-search validates search-str parameter"
    (let [search-tool {:tool-type :symbol-search}]
      (is (= {:search-str "map"}
             (tool-system/validate-inputs search-tool {:search-str "map"})))
      (is (thrown? Exception (tool-system/validate-inputs search-tool {}))))))

(deftest execute-tool-test
  (testing "symbol-completions execution returns completions"
    (with-mocked-core-fns
      #(let [tool-instance (sut/create-symbol-completions-tool *nrepl-client-atom*)
             result (tool-system/execute-tool tool-instance {:prefix "ma"})]
         (is (map? result))
         (is (vector? (:completions result)))
         (is (false? (:error result))))))

  (testing "symbol-metadata execution returns metadata"
    (with-mocked-core-fns
      #(let [tool-instance (sut/create-symbol-metadata-tool *nrepl-client-atom*)
             result (tool-system/execute-tool tool-instance {:symbol "map"})]
         (is (map? result))
         (is (map? (:metadata result)))
         (is (false? (:error result))))))

  (testing "source-code execution returns source"
    (with-mocked-core-fns
      #(let [tool-instance (sut/create-source-code-tool *nrepl-client-atom*)
             result (tool-system/execute-tool tool-instance {:symbol "map"})]
         (is (map? result))
         (is (string? (:source result)))
         (is (false? (:error result)))))))

(deftest format-results-test
  (testing "symbol-completions formatting"
    (let [success-result {:completions ["map" "mapv"] :error false}
          error-result {:error true :message "Error retrieving completions"}
          success-formatted (tool-system/format-results {:tool-type :symbol-completions} success-result)
          error-formatted (tool-system/format-results {:tool-type :symbol-completions} error-result)]
      (is (= {:result ["map" "mapv"] :error false} success-formatted))
      (is (= {:result ["Error retrieving completions"] :error true} error-formatted))))

  (testing "symbol-metadata formatting"
    (let [success-result {:metadata {:name 'map :doc "Doc"} :error false}
          error-result {:error true :message "Symbol not found"}
          success-formatted (tool-system/format-results {:tool-type :symbol-metadata} success-result)
          error-formatted (tool-system/format-results {:tool-type :symbol-metadata} error-result)]
      (is (map? success-formatted))
      (is (= 1 (count (:result success-formatted))))
      (is (false? (:error success-formatted)))
      (is (= {:result ["Symbol not found"] :error true} error-formatted))))

  (testing "source-code formatting"
    (let [success-result {:source "(defn map [f coll] ...)" :error false}
          error-result {:error true :message "Source not found"}
          success-formatted (tool-system/format-results {:tool-type :source-code} success-result)
          error-formatted (tool-system/format-results {:tool-type :source-code} error-result)]
      (is (= {:result ["(defn map [f coll] ...)"] :error false} success-formatted))
      (is (= {:result ["Source not found"] :error true} error-formatted)))))

(deftest tool-execution-test
  (testing "symbol-completions tool execution"
    (let [result (test-tool-execution :symbol-completions {"prefix" "ma"})]
      (is (false? (:error? result)))
      (is (= ["map" "mapv"] (:result result)))))

  (testing "symbol-metadata tool execution"
    (let [result (test-tool-execution :symbol-metadata {"symbol" "map"})]
      (is (false? (:error? result)))
      (is (= 1 (count (:result result))))
      (is (string? (first (:result result))))))

  (testing "symbol-documentation tool execution"
    (let [result (test-tool-execution :symbol-documentation {"symbol" "map"})]
      (is (false? (:error? result)))
      (is (= 1 (count (:result result))))
      (is (string? (first (:result result))))
      (is (str/includes? (first (:result result)) "Doc"))))

  (testing "source-code tool execution"
    (let [result (test-tool-execution :source-code {"symbol" "map"})]
      (is (false? (:error? result)))
      (is (= ["(defn map [f coll] ...)"] (:result result)))))

  (testing "symbol-search tool execution"
    (let [result (test-tool-execution :symbol-search {"search-str" "map"})]
      (is (false? (:error? result)))
      (is (= ["clojure.core/map"] (:result result))))))

(deftest registration-map-test
  (testing "backward compatibility functions return valid registration maps"
    (doseq [[_name-str tool-fn]
            [["symbol-completions" sut/symbol-completions-tool]
             ["symbol-metadata" sut/symbol-metadata-tool]
             ["symbol-documentation" sut/symbol-documentation-tool]
             ["source-code" sut/source-code-tool]
             ["symbol-search" sut/symbol-search-tool]]]
      (let [reg-map (tool-fn *nrepl-client-atom*)]
        (is (map? reg-map))
        (is (string? (:name reg-map)))
        (is (string? (:description reg-map)))
        (is (map? (:schema reg-map)))
        (is (fn? (:tool-fn reg-map)))))))
