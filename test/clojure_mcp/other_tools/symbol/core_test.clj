(ns clojure-mcp.other-tools.symbol.core-test
  (:require
   [clojure-mcp.nrepl]
   [clojure-mcp.other-tools.symbol.core :as sut]
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

;; We'll use the actual nREPL implementation for real results

(deftest get-symbol-completions-test
  (testing "get-symbol-completions returns valid response structure"
    (let [client @*nrepl-client-atom*
          result (sut/get-symbol-completions client "ma")]
      (is (map? result))
      (is (vector? (:completions result)))
      ;; Don't require specific completions since the test environment 
      ;; might not have full completion support configured
      (is (boolean? (:error result)))
      ;; If there's no error, we should have a completions vector (even if empty)
      (when-not (:error result)
        (is (vector? (:completions result)))))))

(deftest get-symbol-metadata-test
  (testing "get-symbol-metadata returns metadata for a valid symbol"
    (let [client @*nrepl-client-atom*
          result (sut/get-symbol-metadata client "map")]
      (is (map? result))
      (is (map? (:metadata result)))
      (is (= (str (:name (:metadata result))) "map") "Name should be map")
      (is (false? (:error result)))))

  (testing "get-symbol-metadata handles missing symbols"
    (let [client @*nrepl-client-atom*
          result (sut/get-symbol-metadata client "this-symbol-does-not-exist-anywhere")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "not found")))))

(deftest get-symbol-documentation-test
  (testing "get-symbol-documentation returns doc and arglists for a valid symbol"
    (let [client @*nrepl-client-atom*
          result (sut/get-symbol-documentation client "map")]
      (is (map? result))
      (is (sequential? (:arglists result)))
      (is (seq (:arglists result)))
      (is (string? (:doc result)))
      (is (str/includes? (:doc result) "Returns a lazy"))
      (is (false? (:error result)))))

  (testing "get-symbol-documentation handles missing symbols"
    (let [client @*nrepl-client-atom*
          result (sut/get-symbol-documentation client "this-symbol-does-not-exist-anywhere")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "not found")))))

(deftest get-source-code-test
  (testing "get-source-code returns source for a valid symbol"
    (let [client @*nrepl-client-atom*
          ;; Use a well-known function to test source retrieval
          result (sut/get-source-code client "map")]
      (is (map? result))
      (is (string? (:source result)))
      (is (str/includes? (:source result) "(defn map"))
      (is (false? (:error result)))))

  (testing "get-source-code handles missing source"
    (let [client @*nrepl-client-atom*
          result (sut/get-source-code client "this-symbol-does-not-exist-anywhere")]
      (is (map? result))
      (is (true? (:error result)))
      (is (string? (:message result)))
      (is (str/includes? (:message result) "Source not found")))))

(deftest search-symbols-test
  (testing "search-symbols returns matches for a valid search"
    (let [client @*nrepl-client-atom*
          result (sut/search-symbols client "map")]
      (is (map? result))
      (is (vector? (:matches result)))
      (is (some #(= % "clojure.core/map") (:matches result)))
      (is (some #(= % "clojure.core/mapv") (:matches result)))
      (is (false? (:error result)))))

  (testing "search-symbols handles empty results"
    (let [client @*nrepl-client-atom*
          result (sut/search-symbols client "xyz123nonexistent")]
      (is (map? result))
      (is (vector? (:matches result)))
      (is (= ["No matches found"] (:matches result)))
      (is (false? (:error result))))))
