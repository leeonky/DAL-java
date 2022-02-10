Feature: chain

  Scenario: return root object when no code
    Given the following json:
    """
    1
    """
    When evaluate by:
    """
    """
    Then the result should:
    """
    : 1
    """
    And the inspect should:
    """
    """

  Scenario Outline: arithmetic chain after arithmetic
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code     | value | inspect    |
      | 1+ 2-3   | 0     | 1 + 2 - 3  |
      | 1+2 *3   | 7     | 1 + 2 * 3  |
      | 4/2-3    | -1    | 4 / 2 - 3  |
      | 4/2- -3  | 5     | 4 / 2 - -3 |
      | 4/2 > -3 | true  | 4 / 2 > -3 |

#  ARITHMETIC, JUDGEMENT, EXPLICIT_PROPERTY, WHICH, SCHEMA
#TODO
#  Scenario: end with .@ is invalid
#    Given the following dal code:
#    """
#      .@ + 0
#    """
#    Then failed to get "expression" node with the following message:
#    """
#    element property needed
#    """
#    And got the following source code information:
#    """
#      .@
#      ^
#    """
