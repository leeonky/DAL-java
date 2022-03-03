Feature: wildcard

  Scenario Outline: force positive verification
    Given the following json:
    """
    <input>
    """
    Then the following verification should pass:
    """
    <operator> *
    """
    And the inspect should:
    """
    <operator> *
    """
    Examples:
      | input | operator |
      | null  | =        |
      | 1     | :        |

  Scenario: should not evaluate when compare with wildcard
    * the following verification should pass:
    """
    [-1]: *
    """
