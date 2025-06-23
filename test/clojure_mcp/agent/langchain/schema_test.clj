(ns clojure-mcp.agent.langchain.schema-test
  (:require [clojure-mcp.agent.langchain.schema :as schema]
            [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]])
  (:import (dev.langchain4j.agent.tool ToolSpecification)
           (dev.langchain4j.model.chat.request.json
            JsonAnyOfSchema
            JsonArraySchema
            JsonBooleanSchema
            JsonEnumSchema
            JsonIntegerSchema
            JsonNumberSchema
            JsonObjectSchema
            JsonSchemaElement JsonStringSchema)
           (java.util Map)))

(deftest test-simple-schema-types
  (testing "Converting primitive type schemas"
    (let [string-schema {:type :string, :description "A simple string"}
          number-schema {:type :number, :description "A simple number"}
          integer-schema {:type :integer, :description "A simple integer"}
          boolean-schema {:type :boolean, :description "A simple boolean"}

          converted-string (schema/edn->sch string-schema)
          converted-number (schema/edn->sch number-schema)
          converted-integer (schema/edn->sch integer-schema)
          converted-boolean (schema/edn->sch boolean-schema)]

      (is (instance? JsonStringSchema converted-string))
      (is (= "A simple string" (.description converted-string)))

      (is (instance? JsonNumberSchema converted-number))
      (is (= "A simple number" (.description converted-number)))

      (is (instance? JsonIntegerSchema converted-integer))
      (is (= "A simple integer" (.description converted-integer)))

      (is (instance? JsonBooleanSchema converted-boolean))
      (is (= "A simple boolean" (.description converted-boolean))))))

(deftest test-enum-schemas
  (testing "Converting enum schema"
    (let [enum-schema {:enum ["read" "write" "append"]}
          converted-enum (schema/edn->sch enum-schema)]

      (is (instance? JsonEnumSchema converted-enum))
      (is (= ["read" "write" "append"] (.enumValues converted-enum))))))

(deftest test-array-schemas
  (testing "Converting array schemas with different item types"
    (let [string-array-schema {:type :array, :items {:type :string}}
          number-array-schema {:type :array, :items {:type :number}}
          integer-array-schema {:type :array, :items {:type :integer}}
          boolean-array-schema {:type :array, :items {:type :boolean}}

          converted-string-array (schema/edn->sch string-array-schema)
          converted-number-array (schema/edn->sch number-array-schema)
          converted-integer-array (schema/edn->sch integer-array-schema)
          converted-boolean-array (schema/edn->sch boolean-array-schema)]

      (is (instance? JsonArraySchema converted-string-array))
      (is (instance? JsonStringSchema (.items converted-string-array)))

      (is (instance? JsonArraySchema converted-number-array))
      (is (instance? JsonNumberSchema (.items converted-number-array)))

      (is (instance? JsonArraySchema converted-integer-array))
      (is (instance? JsonIntegerSchema (.items converted-integer-array)))

      (is (instance? JsonArraySchema converted-boolean-array))
      (is (instance? JsonBooleanSchema (.items converted-boolean-array))))))

(deftest test-object-schemas
  (testing "Converting object schemas with required fields"
    (let [object-schema {:type :object
                         :description "A test object"
                         :properties {:name {:type :string, :description "The name"}
                                      :age {:type :integer, :description "The age"}}
                         :required [:name]}
          converted-object ^JsonObjectSchema (schema/edn->sch object-schema)
          properties ^Map (.properties converted-object)]

      (is (instance? JsonObjectSchema converted-object))
      (is (= "A test object" (.description converted-object)))
      (is (= ["name"] (seq (.required converted-object))))
      (is (instance? JsonStringSchema (get properties "name")))
      (is (instance? JsonIntegerSchema (get properties "age")))
      (is (= "The name" (.description ^JsonSchemaElement (get properties "name"))))
      (is (= "The age" (.description ^JsonSchemaElement (get properties "age")))))))

(deftest test-nested-object-schemas
  (testing "Converting nested object schemas"
    (let [nested-schema {:type :object
                         :description "A nested object schema"
                         :properties {:user {:type :object
                                             :properties {:name {:type :string}
                                                          :details {:type :object
                                                                    :properties {:age {:type :integer}
                                                                                 :active {:type :boolean}}
                                                                    :required [:active]}}
                                             :required [:name]}
                                      :metadata {:type :object
                                                 :properties {:tags {:type :array
                                                                     :items {:type :string}}}}}
                         :required [:user]}
          converted-nested ^JsonObjectSchema (schema/edn->sch nested-schema)
          properties ^Map (.properties converted-nested)
          user-schema ^JsonObjectSchema (get properties "user")
          user-properties ^Map (.properties user-schema)
          details-schema ^JsonObjectSchema (get user-properties "details")
          details-properties (.properties details-schema)
          metadata-schema ^JsonObjectSchema (get properties "metadata")
          metadata-properties (.properties metadata-schema)
          tags-schema ^JsonArraySchema (get metadata-properties "tags")]

      (is (instance? JsonObjectSchema converted-nested))
      (is (= "A nested object schema" (.description converted-nested)))
      (is (= ["user"] (seq (.required converted-nested))))

      (is (instance? JsonObjectSchema user-schema))
      (is (= ["name"] (seq (.required user-schema))))
      (is (instance? JsonStringSchema (get user-properties "name")))

      (is (instance? JsonObjectSchema details-schema))
      (is (= ["active"] (seq (.required details-schema))))
      (is (instance? JsonIntegerSchema (get details-properties "age")))
      (is (instance? JsonBooleanSchema (get details-properties "active")))

      (is (instance? JsonObjectSchema metadata-schema))
      (is (instance? JsonArraySchema tags-schema))
      (is (instance? JsonStringSchema (.items tags-schema))))))

(deftest test-complex-combined-schemas
  (testing "Converting complex schemas combining multiple types"
    (let [complex-schema {:type :object
                          :description "A complex schema"
                          :properties {:id {:type :string}
                                       :tags {:type :array
                                              :items {:type :string}}
                                       :config {:type :object
                                                :properties {:mode {:enum ["simple" "advanced" "expert"]}
                                                             :limits {:type :object
                                                                      :properties {:max {:type :integer}
                                                                                   :min {:type :integer}}
                                                                      :required [:max :min]}
                                                             :flags {:type :array
                                                                     :items {:type :boolean}}}
                                                :required [:mode]}}
                          :required [:id :config]}
          converted-complex (schema/edn->sch complex-schema)
          properties (.properties converted-complex)
          tags-schema ^JsonArraySchema (get properties "tags")
          config-schema ^JsonObjectSchema (get properties "config")
          config-properties (.properties config-schema)
          mode-schema ^JsonEnumSchema (get config-properties "mode")
          limits-schema ^JsonObjectSchema (get config-properties "limits")
          limits-properties (.properties limits-schema)
          flags-schema ^JsonArraySchema (get config-properties "flags")]

      (is (instance? JsonObjectSchema converted-complex))
      (is (= "A complex schema" (.description converted-complex)))
      (is (= #{"id" "config"} (set (.required converted-complex))))

      (is (instance? JsonStringSchema (get properties "id")))
      (is (instance? JsonArraySchema tags-schema))
      (is (instance? JsonStringSchema (.items tags-schema)))

      (is (instance? JsonObjectSchema config-schema))
      (is (= ["mode"] (seq (.required config-schema))))

      (is (instance? JsonEnumSchema mode-schema))
      (is (= ["simple" "advanced" "expert"] (.enumValues mode-schema)))

      (is (instance? JsonObjectSchema limits-schema))
      (is (= #{"max" "min"} (set (.required limits-schema))))
      (is (instance? JsonIntegerSchema (get limits-properties "max")))
      (is (instance? JsonIntegerSchema (get limits-properties "min")))

      (is (instance? JsonArraySchema flags-schema))
      (is (instance? JsonBooleanSchema (.items flags-schema))))))

(deftest test-string-json-conversion
  (testing "Converting JSON string schemas"
    (let [json-schema-str (json/write-str {:type "object"
                                           :properties {:name {:type "string"}
                                                        :age {:type "integer"}}
                                           :required ["name"]})
          schema-map (json/read-str json-schema-str :key-fn keyword)
          converted-schema (schema/edn->sch schema-map)
          properties (.properties converted-schema)]

      (is (instance? JsonObjectSchema converted-schema))
      (is (= ["name"] (seq (.required converted-schema))))
      (is (instance? JsonStringSchema (get properties "name")))
      (is (instance? JsonIntegerSchema (get properties "age"))))))

(deftest test-tool-specification-integration
  (testing "Using schema conversion with ToolSpecification"
    (let [schema {:type :object
                  :properties {:prompt {:type :string
                                        :description "The prompt to send"}
                               :options {:type :array
                                         :items {:type :string}
                                         :description "List of options"}}
                  :required [:prompt]}
          json-schema-str (json/write-str schema)]

      ;; Test with direct EDN schema
      (let [edn-converted (schema/edn->sch schema)
            properties (.properties edn-converted)]
        (is (instance? JsonObjectSchema edn-converted))
        (is (= ["prompt"] (seq (.required edn-converted))))
        (is (instance? JsonStringSchema (get properties "prompt")))
        (is (instance? JsonArraySchema (get properties "options")))
        (is (instance? JsonStringSchema (.items ^JsonArraySchema (get properties "options")))))

      ;; Test with JSON string after parsing
      (let [json-converted (schema/edn->sch (json/read-str json-schema-str :key-fn keyword))
            properties (.properties json-converted)]
        (is (instance? JsonObjectSchema json-converted))
        (is (= ["prompt"] (seq (.required json-converted))))
        (is (instance? JsonStringSchema (get properties "prompt")))
        (is (instance? JsonArraySchema (get properties "options")))
        (is (instance? JsonStringSchema (.items ^JsonArraySchema (get properties "options"))))))))

(deftest test-mixed-types
  (testing "Converting mixed type schemas"
    (let [mixed-schema {:type ["string" "number"]
                        :description "Can be string or number"}
          converted-mixed (schema/edn->sch mixed-schema)]

      (is (instance? JsonAnyOfSchema converted-mixed))
      (is (= "Can be string or number" (.description converted-mixed)))

      (let [any-of-schemas (vec (.anyOf converted-mixed))]
        (is (= 2 (count any-of-schemas)))
        (is (instance? JsonStringSchema (first any-of-schemas)))
        (is (instance? JsonNumberSchema (second any-of-schemas))))))

  (testing "Mixed types including object and array"
    (let [mixed-schema {:type ["object" "array" "string" "number" "boolean"]}
          converted-mixed (schema/edn->sch mixed-schema)]

      (is (instance? JsonAnyOfSchema converted-mixed))

      (let [any-of-schemas (vec (.anyOf converted-mixed))]
        (is (= 5 (count any-of-schemas)))
        (is (instance? JsonObjectSchema (nth any-of-schemas 0)))
        (is (instance? JsonArraySchema (nth any-of-schemas 1)))
        (is (instance? JsonStringSchema (nth any-of-schemas 2)))
        (is (instance? JsonNumberSchema (nth any-of-schemas 3)))
        (is (instance? JsonBooleanSchema (nth any-of-schemas 4))))))

  (testing "Mixed types with null (should be skipped)"
    (let [mixed-schema {:type ["string" "null" "number"]}
          converted-mixed (schema/edn->sch mixed-schema)]

      (is (instance? JsonAnyOfSchema converted-mixed))

      ;; null should be skipped, so only 2 schemas
      (let [any-of-schemas (vec (.anyOf converted-mixed))]
        (is (= 2 (count any-of-schemas)))
        (is (instance? JsonStringSchema (first any-of-schemas)))
        (is (instance? JsonNumberSchema (second any-of-schemas)))))))

(deftest test-array-with-mixed-type-items
  (testing "Array schema with mixed type items"
    (let [array-schema {:type :array
                        :items {:type ["string" "number"]}
                        :description "Array of strings or numbers"}
          converted-array ^JsonArraySchema (schema/edn->sch array-schema)]

      (is (instance? JsonArraySchema converted-array))
      (is (instance? JsonAnyOfSchema (.items converted-array)))

      (let [items-schema ^JsonAnyOfSchema (.items converted-array)
            any-of-schemas (vec (.anyOf items-schema))]
        (is (= 2 (count any-of-schemas)))
        (is (instance? JsonStringSchema (first any-of-schemas)))
        (is (instance? JsonNumberSchema (second any-of-schemas)))))))

(deftest test-any-type-schema
  (testing "Any type schema helper function"
    (let [any-schema (schema/any-type-schema)]

      (is (instance? JsonAnyOfSchema any-schema))

      (let [any-of-schemas (vec (.anyOf any-schema))]
        (is (= 5 (count any-of-schemas)))
        (is (instance? JsonStringSchema (nth any-of-schemas 0)))
        (is (instance? JsonNumberSchema (nth any-of-schemas 1)))
        (is (instance? JsonBooleanSchema (nth any-of-schemas 2)))
        (is (instance? JsonObjectSchema (nth any-of-schemas 3)))
        (is (instance? JsonArraySchema (nth any-of-schemas 4)))))))

(deftest test-scratch-pad-schema
  (testing "Complex scratch pad schema with mixed types"
    (let [scratch-pad-schema {:type :object
                              :properties {:op {:enum ["set_path" "get_path" "delete_path" "inspect"]}
                                           :path {:type :array
                                                  :items {:type ["string" "number"]}
                                                  :description "Path to the data location"}
                                           :value {:type ["object" "array" "string" "number" "boolean"]
                                                   :description "Value to store (for set_path)"}
                                           :explanation {:type :string
                                                         :description "Explanation of why this operation is being performed"}
                                           :depth {:type :number
                                                   :description "(Optional) For inspect operation"}}
                              :required [:op :explanation]
                              :description "Persistent scratch pad for storing structured data"}
          converted-schema (schema/edn->sch scratch-pad-schema)
          properties (.properties converted-schema)]

      (is (instance? JsonObjectSchema converted-schema))
      (is (= "Persistent scratch pad for storing structured data" (.description converted-schema)))
      (is (= #{"op" "explanation"} (set (.required converted-schema))))

      ;; Check op enum
      (let [op-schema ^JsonEnumSchema (get properties "op")]
        (is (instance? JsonEnumSchema op-schema))
        (is (= ["set_path" "get_path" "delete_path" "inspect"] (vec (.enumValues op-schema)))))

      ;; Check path array with mixed items
      (let [path-schema ^JsonArraySchema (get properties "path")
            path-items ^JsonAnyOfSchema (.items path-schema)]
        (is (instance? JsonArraySchema path-schema))
        (is (instance? JsonAnyOfSchema path-items))
        (let [item-schemas (vec (.anyOf path-items))]
          (is (= 2 (count item-schemas)))
          (is (instance? JsonStringSchema (first item-schemas)))
          (is (instance? JsonNumberSchema (second item-schemas)))))

      ;; Check value with multiple types
      (let [value-schema ^JsonAnyOfSchema (get properties "value")]
        (is (instance? JsonAnyOfSchema value-schema))
        (is (= "Value to store (for set_path)" (.description value-schema)))
        (let [value-schemas (vec (.anyOf value-schema))]
          (is (= 5 (count value-schemas)))))

      ;; Check other fields
      (is (instance? JsonStringSchema (get properties "explanation")))
      (is (instance? JsonNumberSchema (get properties "depth"))))))

(deftest test-error-handling
  (testing "Invalid schema dispatch"
    (is (thrown-with-msg? Exception #"By JSON data"
                          (schema/edn->sch {:invalid "schema"}))))

  (testing "Missing required fields for array"
    (is (thrown? AssertionError
                 (schema/edn->sch {:type :array}))))

  (testing "Missing required fields for object"
    (is (thrown? AssertionError
                 (schema/edn->sch {:type :object}))))

  (testing "Empty enum"
    (is (thrown? AssertionError
                 (schema/edn->sch {:enum []}))))

  (testing "Non-string enum values"
    (is (thrown? AssertionError
                 (schema/edn->sch {:enum [1 2 3]})))))

(deftest test-tool-specification-with-mixed-types
  (testing "Creating ToolSpecification with mixed types"

    (let [schema {:type :object
                  :properties {:action {:enum ["read" "write"]}
                               :target {:type ["string" "number"]
                                        :description "Can be filename or ID"}
                               :data {:type ["object" "array" "string"]
                                      :description "Flexible data field"}}
                  :required [:action :target]}
          params (schema/edn->sch schema)
          tool-spec (-> (ToolSpecification/builder)
                        (.name "flexible_tool")
                        (.description "A tool with mixed type parameters")
                        (.parameters params)
                        .build)]

      (is (= "flexible_tool" (.name tool-spec)))
      (is (= "A tool with mixed type parameters" (.description tool-spec)))
      (is (instance? JsonObjectSchema (.parameters tool-spec)))

      (let [properties (.properties (.parameters tool-spec))
            action-schema (get properties "action")
            target-schema ^JsonAnyOfSchema (get properties "target")
            data-schema ^JsonAnyOfSchema (get properties "data")]

        (is (instance? JsonEnumSchema action-schema))
        (is (instance? JsonAnyOfSchema target-schema))
        (is (instance? JsonAnyOfSchema data-schema))
        (is (= "Can be filename or ID" (.description target-schema)))
        (is (= "Flexible data field" (.description data-schema)))))))








