Feature: group

  Scenario Outline: support group with << >> and verification with same value
    Given the following json:
    """
    {
      "a": 1,
      "b": 1,
      "c": 2
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
    <<a, c>><opt> 1
    """
    Then failed with the message:
    """
    Expecting java.lang.Integer
    <2>
    to <text> java.lang.Integer
    <1>
    but was not
    """
    And got the following notation:
    """
    <<a, c>><opt> 1
         ^
              ^
    """
    Examples:
      | opt | text        |
      | :   | match       |
      | =   | be equal to |

