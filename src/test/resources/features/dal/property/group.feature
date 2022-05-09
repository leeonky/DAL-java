Feature: group

  Scenario Outline: support group with << >> and verification with same value
    Given the following json:
    """
    {
      "a": 1,
      "b": 1,
      "c": {
        "value": 2
      }
    }
    """
    Then the following verification should pass:
    """
    <<a b>><opt> 1
    """
    And the inspect should:
    """
    <<a, b>><opt> 1
    """
    And evaluate by:
    """
    <<a, c.value>><opt> 1
    """
    Then failed with the message:
    """
    Expected to <text>: java.lang.Integer
    <1>
    Actual: java.lang.Integer
    <2>
    """
    And got the following notation:
    """
    <<a, c.value>><opt> 1
           ^
                    ^
    """
    Examples:
      | opt | text        |
      | :   | match       |
      | =   | be equal to |

  Scenario Outline: invoke property of group should also a group
    Given the following json:
    """
    {
      "a": {
        "value": 1
      },
      "b": {
        "value": 1
      },
      "c": {
        "value": 2
      }
    }
    """
    Then the following verification should pass:
    """
    <<a b>>.value<opt> 1
    """
    And the inspect should:
    """
    <<a, b>>.value<opt> 1
    """
    And evaluate by:
    """
    <<a, c>>.value<opt> 1
    """
    Then failed with the message:
    """
    Expected to <text>: java.lang.Integer
    <1>
    Actual: java.lang.Integer
    <2>
    """
    And got the following notation:
    """
    <<a, c>>.value<opt> 1
         ^
                    ^
    """
    Examples:
      | opt | text        |
      | :   | match       |
      | =   | be equal to |
