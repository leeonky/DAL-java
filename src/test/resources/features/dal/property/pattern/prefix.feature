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

  Scenario: should not take number key as string key in prefix
    Given the following json:
    """
    {
      "value0": "A",
      "value1": "B",
      "value2": "C"
    }
    """
    When evaluate by:
    """
    = {
      value{}: {
        0: null
        1: null
        2: null
      }
    }
    """
    Then failed with the message:
    """
    Unexpected fields `value0`, `value1`, `value2`
    """


#  TODO field alias not support yet
#  TODO bracket string relax not support yet
