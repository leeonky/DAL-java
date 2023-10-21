Feature: single schema when verification failed

  Background:
    Given the following schema class:
    """
    public class IdZero implements Schema {
        public int id = 0;
    }
    """

  Scenario: verify data matches single schema
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is IdZero
    """

  Scenario: raise error when unexpected field
    When the following json:
    """
      {
        "id": 0,
        "unexpected field": 1
      }
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expected to match schema `IdZero` but was not
        Unexpected field `unexpected field` for schema IdZero[#package#IdZero]
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when expected field is not exist
    When the following json:
    """
      {}
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expected to match schema `IdZero` but was not
        Expected field `id` to be in type IdZero[#package#IdZero], but does not exist
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when schema field type is not matched
    When the following json:
    """
      {
        "id": "0"
      }
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expected to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.String
        <0>
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when schema field value is not matched
    When the following json:
    """
      {
        "id": 2
      }
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expected to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.Integer
        <2>
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when schema field type without value is not matched
    Given the following schema class:
    """
    public class StringId implements Schema {
        public String id;
    }
    """
    When the following json:
    """
      {
        "id": "0"
      }
    """
    Then the following verification should pass:
    """
      is StringId
    """
    When the following json:
    """
      {
        "id": 0
      }
    """
    When evaluate by:
    """
      is StringId
    """
    Then failed with the message:
    """
    Expected to match schema `StringId` but was not
        Expected field `.id` to be java.lang.String
        Actual: java.lang.Integer
        <0>
    """
    And got the following notation:
    """
      is StringId
         ^
    """

  Scenario: @AllowNull on field
    Given the following json:
    """
      {
        "value": null
      }
    """
    Given the following schema class:
    """
    public class Data implements Schema {
        @AllowNull
        public Integer value;
    }
    """
    Then the following verification should pass:
    """
    is Data
    """

  Scenario: compare type in un-boxed type
    Given the following schema class:
    """
    public class Id implements Schema {
        public int id;
    }
    """
    And the following schema class:
    """
    public class MapId implements Schema {
      public Map<String, IdZero> value;
    }
    """
    When the following json:
    """
    {
      "value": {
        "object": {
          "id": 0
        }
      }
    }
    """

    Then the following verification should pass:
    """
    is MapId
    """

  Scenario: input value is Formatter
    Given the following json:
    """
      "string"
    """
    Then the following verification should pass:
    """
    is String
    """

  Scenario: raise error when element size is different
    Given the following schema class:
    """
    public class BeanSchema implements Schema {
        public String value1[] = new String [] {"hello", "world"};
    }
    """
    Given the following json:
    """
    {
      "value1": ["hello"]
    }
    """
    When evaluate by:
    """
    is BeanSchema
    """
    Then failed with the message:
    """
    Expected to match schema `BeanSchema` but was not
        Collection Field `.value1` size was only <1>, expected too more
    """
    Given the following json:
    """
    {
      "value1": ["hello", "world", "!"]
    }
    """
    When evaluate by:
    """
    is BeanSchema
    """
    Then failed with the message:
    """
    Expected to match schema `BeanSchema` but was not
        Expected collection field `.value1` to be size <2>, but too many elements
    """
