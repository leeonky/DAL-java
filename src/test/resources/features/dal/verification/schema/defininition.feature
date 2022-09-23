Feature: define schema

  Scenario: map element is schema
    Given the following schema class:
    """
    public class IdZero implements Schema {
        public String id = "0";
    }
    """
    Given the following json:
    """
    {
      "value": {
        "object": {
          "id": "0"
        }
      }
    }
    """
    And the following schema class:
    """
    public class MapIdZero implements Schema {
        public Map<String, IdZero> value;
    }
    """
    And the following verification should pass:
    """
    is MapIdZero
    """
    Given the following json:
    """
    {
      "value": {
        "object": {
          "id": "1"
        }
      }
    }
    """
    When evaluate by:
    """
    is MapIdZero
    """
    Then failed with the message:
    """
    Expected to match schema `MapIdZero` but was not
        Expected field `.value.object.id` to be java.lang.String
        <0>
        Actual: java.lang.String
        <1>
    """
    And got the following notation:
    """
    is MapIdZero
       ^
    """

  Scenario: list element is schema
    Given the following schema class:
    """
    public class IdZero implements Schema {
        public String id = "0";
    }
    """
    Given the following json:
    """
    {
      "value": [{
        "id": "0"
      }]
    }
    """
    And the following schema class:
    """
    public class ListIdZero implements Schema {
        public List<IdZero> value;
    }
    """
    And the following verification should pass:
    """
    is ListIdZero
    """
    Given the following json:
    """
    {
      "value": [{
        "id": "0"
      },{
        "id": "1"
      }]
    }
    """
    When evaluate by:
    """
    is ListIdZero
    """
    Then failed with the message:
    """
    Expected to match schema `ListIdZero` but was not
        Expected field `.value[1].id` to be java.lang.String
        <0>
        Actual: java.lang.String
        <1>
    """
    And got the following notation:
    """
    is ListIdZero
       ^
    """
