(ns clojure-mcp.other-tools.namespace.core-test
  (:require
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.other-tools.namespace.core :as sut]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [nrepl.server :as nrepl-server]))

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

;; Helper function to evaluate code for tests
(defn eval-helper [client code]
  (nrepl/tool-eval-code client code))

(deftest get-current-namespace-test
  (testing "get-current-namespace returns the current namespace when available"
    ;; First set a known current namespace
    (nrepl/eval-code @*nrepl-client-atom* "(in-ns 'user)" identity)
    (let [result (sut/get-current-namespace @*nrepl-client-atom*)]
      (is (map? result))
      (is (= "user" (:namespace result)))
      (is (false? (:error result)))))

  (testing "get-current-namespace with custom namespace"
    ;; Create and switch to a test namespace
    (nrepl/eval-code @*nrepl-client-atom* "(create-ns 'test-ns)" identity)
    (nrepl/eval-code @*nrepl-client-atom* "(in-ns 'test-ns)" identity)
    (let [result (sut/get-current-namespace @*nrepl-client-atom*)]
      (is (map? result))
      (is (= "test-ns" (:namespace result)))
      (is (false? (:error result))))
    ;; Switch back to user namespace for subsequent tests
    (nrepl/eval-code @*nrepl-client-atom* "(in-ns 'user)" identity)))

(deftest get-all-namespaces-test
  (testing "get-all-namespaces returns namespaces when available"
    (let [result (sut/get-all-namespaces @*nrepl-client-atom* eval-helper)]
      (is (map? result))
      (is (vector? (:namespaces result)))
      (is (false? (:error result)))
      ;; Check for some common namespaces that should be there
      (is (some #(= "clojure.core" %) (:namespaces result)))
      (is (some #(= "clojure.string" %) (:namespaces result)))
      (is (some #(= "user" %) (:namespaces result))))))

(deftest get-vars-in-namespace-test
  (testing "get-vars-in-namespace returns vars for clojure.string"
    (let [result (sut/get-vars-in-namespace @*nrepl-client-atom* eval-helper "clojure.string")]
      (is (map? result))
      (is (vector? (:vars result)))
      (is (false? (:error result)))
      ;; Check some common functions in clojure.string
      (is (some #(= 'join (:name %)) (:vars result)))
      (is (some #(= 'split (:name %)) (:vars result)))))

  (testing "get-vars-in-namespace returns error for nonexistent namespace"
    (let [result (sut/get-vars-in-namespace @*nrepl-client-atom* eval-helper "nonexistent.namespace")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "not found")))))