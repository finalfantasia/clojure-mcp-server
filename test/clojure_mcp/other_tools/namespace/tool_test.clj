(ns clojure-mcp.other-tools.namespace.tool-test
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.other-tools.namespace.tool :as sut]
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

;; Helper to directly invoke the tool function from our tool registration map
(defn test-tool-execution [tool-name inputs]
  (let [tool-instance (case tool-name
                        :current-namespace (sut/create-current-namespace-tool *nrepl-client-atom*)
                        :list-namespaces (sut/create-list-namespaces-tool *nrepl-client-atom*)
                        :list-vars-in-namespace (sut/create-list-vars-tool *nrepl-client-atom*))
        reg-map (tool-system/registration-map tool-instance)
        tool-fn (:tool-fn reg-map)
        promise-result (promise)]
    (tool-fn nil inputs
             (fn [result error?]
               (deliver promise-result {:result result :error? error?})))
    @promise-result))

;; Helper to test just the individual steps of the pipeline
(defn test-pipeline-steps [tool-type inputs]
  (let [tool-instance (case tool-type
                        :current-namespace (sut/create-current-namespace-tool *nrepl-client-atom*)
                        :list-namespaces (sut/create-list-namespaces-tool *nrepl-client-atom*)
                        :list-vars-in-namespace (sut/create-list-vars-tool *nrepl-client-atom*))
        validated (tool-system/validate-inputs tool-instance inputs)
        execution-result (tool-system/execute-tool tool-instance validated)
        formatted-result (tool-system/format-results tool-instance execution-result)]
    formatted-result))

(deftest tool-name-test
  (testing "Tool names are correct"
    (is (= "current_namespace"
           (tool-system/tool-name {:tool-type :current-namespace})))
    (is (= "clojure_list_namespaces"
           (tool-system/tool-name {:tool-type :list-namespaces})))
    (is (= "clojure_list_vars_in_namespace"
           (tool-system/tool-name {:tool-type :list-vars-in-namespace})))))

(deftest tool-description-test
  (testing "Tool descriptions are non-empty"
    (doseq [tool-type [:current-namespace :list-namespaces :list-vars-in-namespace]]
      (let [description (tool-system/tool-description {:tool-type tool-type})]
        (is (string? description))
        (is (not (str/blank? description)))))))

(deftest tool-schema-test
  (testing "current-namespace schema is correct"
    (let [schema (tool-system/tool-schema {:tool-type :current-namespace})]
      (is (map? schema))
      (is (= :object (:type schema)))))

  (testing "list-namespaces schema is correct"
    (let [schema (tool-system/tool-schema {:tool-type :list-namespaces})]
      (is (map? schema))
      (is (= :object (:type schema)))))

  (testing "list-vars-in-namespace schema is correct"
    (let [schema (tool-system/tool-schema {:tool-type :list-vars-in-namespace})]
      (is (map? schema))
      (is (= :object (:type schema)))
      (is (contains? (:properties schema) :namespace))
      (is (= [:namespace] (:required schema))))))

(deftest validate-inputs-test
  (testing "current-namespace and list-namespaces accept empty inputs"
    (let [current-ns-tool {:tool-type :current-namespace}
          list-ns-tool {:tool-type :list-namespaces}]
      (is (= {} (tool-system/validate-inputs current-ns-tool {})))
      (is (= {} (tool-system/validate-inputs list-ns-tool {})))))

  (testing "list-vars-in-namespace validates namespace parameter"
    (let [list-vars-tool {:tool-type :list-vars-in-namespace
                          :nrepl-client-atom *nrepl-client-atom*}]
      (is (= {:namespace "clojure.string"}
             (tool-system/validate-inputs list-vars-tool {:namespace "clojure.string"})))
      (is (thrown? Exception (tool-system/validate-inputs list-vars-tool {}))))))

(deftest execute-tool-test
  (testing "Set current namespace before testing"
    (nrepl/eval-code @*nrepl-client-atom* "(in-ns 'user)" identity))

  (testing "current-namespace execution returns current namespace"
    (let [tool-instance (sut/create-current-namespace-tool *nrepl-client-atom*)
          result (tool-system/execute-tool tool-instance {})]
      (is (map? result))
      (is (= "user" (:namespace result)))
      (is (false? (:error result)))))

  (testing "list-namespaces execution returns list of namespaces"
    (let [tool-instance (sut/create-list-namespaces-tool *nrepl-client-atom*)
          result (tool-system/execute-tool tool-instance {})]
      (is (map? result))
      (is (vector? (:namespaces result)))
      (is (some #(= "clojure.core" %) (:namespaces result)))
      (is (false? (:error result)))))

  (testing "list-vars-in-namespace execution returns vars for valid namespace"
    (let [tool-instance (sut/create-list-vars-tool *nrepl-client-atom*)
          result (tool-system/execute-tool tool-instance {:namespace "clojure.string"})]
      (is (map? result))
      (is (vector? (:vars result)))
      (is (map? (first (:vars result))))
      (is (false? (:error result)))))

  (testing "list-vars-in-namespace execution returns error for invalid namespace"
    (let [tool-instance (sut/create-list-vars-tool *nrepl-client-atom*)
          result (tool-system/execute-tool tool-instance {:namespace "nonexistent.ns"})]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result))))))

(deftest format-results-test
  (testing "current-namespace formatting"
    (let [success-result {:namespace "user" :error false}
          error-result {:error true :message "No current namespace"}
          success-formatted (tool-system/format-results {:tool-type :current-namespace} success-result)
          error-formatted (tool-system/format-results {:tool-type :current-namespace} error-result)]
      (is (= {:result ["user"] :error false} success-formatted))
      (is (= {:result ["No current namespace"] :error true} error-formatted))))

  (testing "list-namespaces formatting"
    (let [success-result {:namespaces ["clojure.core" "clojure.string"] :error false}
          error-result {:error true :message "Error retrieving namespaces"}
          success-formatted (tool-system/format-results {:tool-type :list-namespaces} success-result)
          error-formatted (tool-system/format-results {:tool-type :list-namespaces} error-result)]
      (is (= {:result ["clojure.core" "clojure.string"] :error false} success-formatted))
      (is (= {:result ["Error retrieving namespaces"] :error true} error-formatted))))

  (testing "list-vars-in-namespace formatting"
    (let [var-meta {:name 'join :arglists '([coll] [separator coll]) :doc "Joins strings" :ns "clojure.string"}
          success-result {:vars [var-meta] :error false}
          error-result {:error true :message "Namespace not found"}
          success-formatted (tool-system/format-results {:tool-type :list-vars-in-namespace} success-result)
          error-formatted (tool-system/format-results {:tool-type :list-vars-in-namespace} error-result)
          expected-format "join\n  ([coll] [separator coll])\n  Joins strings"]
      (is (= {:result [expected-format] :error false} success-formatted))
      (is (= {:result ["Namespace not found"] :error true} error-formatted)))))

(deftest tool-execution-test
  (testing "Set current namespace before testing"
    (nrepl/eval-code @*nrepl-client-atom* "(in-ns 'user)" identity))

  (testing "current-namespace tool execution"
    (let [result (test-tool-execution :current-namespace {})]
      (is (false? (:error? result)))
      (is (= ["user"] (:result result)))))

  (testing "list-namespaces tool execution"
    (let [result (test-tool-execution :list-namespaces {})]
      (is (false? (:error? result)))
      (is (some #(= "clojure.core" %) (:result result)))
      (is (some #(= "clojure.string" %) (:result result)))))

  (testing "list-vars-in-namespace tool execution with valid namespace"
    (let [result (test-tool-execution :list-vars-in-namespace {"namespace" "clojure.string"})]
      (is (false? (:error? result)))
      (is (vector? (:result result)))
      (is (= 1 (count (:result result)))) ; Now we return a single formatted string
      (is (string? (first (:result result))))
      ; Check that the output contains key function names
      (let [output (first (:result result))]
        (is (str/includes? output "join"))
        (is (str/includes? output "split"))
        ; Check that formatting is working (contains argument lists)
        (is (str/includes? output "([coll]"))
        ; Check that we have separators between functions
        (is (str/includes? output "---")))))

  (testing "list-vars-in-namespace tool execution with invalid namespace"
    (let [result (test-tool-execution :list-vars-in-namespace {"namespace" "nonexistent.ns"})]
      (is (true? (:error? result)))
      (is (string? (first (:result result))))
      (is (str/includes? (first (:result result)) "not found")))))

(deftest registration-map-test
  (testing "backward compatibility functions return valid registration maps"
    (doseq [[_fn-name tool-fn]
            [["current-namespace" sut/current-namespace-tool]
             ["list-namespaces" sut/list-namespaces-tool]
             ["list-vars-in-namespace" sut/list-vars-in-namespace-tool]]]
      (let [reg-map (tool-fn *nrepl-client-atom*)]
        (is (map? reg-map))
        (is (string? (:name reg-map)))
        (is (string? (:description reg-map)))
        (is (map? (:schema reg-map)))
        (is (fn? (:tool-fn reg-map)))))))
