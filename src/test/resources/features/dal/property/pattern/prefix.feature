Feature: prefix

  Scenario Outline: filter object by property pattern with prefix
    Given the following json:
    """
    {
      "valueA": {
        "value": "A"
      },
      "valueB": {
        "value": "B"
      },
      "valueC": {
        "value": "C"
      }
    }
    """
    When evaluate by:
    """
    <propertyType>value{}
    """
    Then the result should:
    """
    = {
      "a": {
        "value": "A"
      },
      "b": {
        "value": "B"
      },
      "c": {
        "value": "C"
      }
    }
    """
    And the inspect should:
    """
    <propertyType>value{}
    """
    Examples:
      | propertyType |
      |              |
      | .            |

  Scenario: check all property when use prefix
    Given the following json:
    """
    {
      "valueA": {
        "value": "A"
      },
      "valueB": {
        "value": "B"
      },
      "valueC": {
        "value": "C"
      }
    }
    """
    Then the following verification should pass:
    """
    = {
      value{}: {
        a.value: A
        b.value: B
        c.value: C
      }
    }
    """

  Scenario: {} as this reference
    Given the following java class:
    """
    public class Data {
      public int value = 10;
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    : {
      {}: {
        value: 10
      }
    }
    """
    And the inspect should:
    """
    : {{}: {value: 10}}
    """

#  TODO field alias not support yet
#  TODO bracket string relax not support yet
