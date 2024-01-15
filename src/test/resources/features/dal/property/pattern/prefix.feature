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
      "A": {
        "value": "A"
      },
      "B": {
        "value": "B"
      },
      "C": {
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
        A.value: A
        B.value: B
        C.value: C
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

  Scenario: should skip number key
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, Object> data = new java.util.HashMap<Object, Object>() {{
        put(0, 0);
        put("ab", "b");
        put("ac", "c");
      }};
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data: {
      a{}= {
        b: b
        c: c
      }
    }
    """

#  TODO field alias not support yet
#  TODO bracket string relax not support yet
