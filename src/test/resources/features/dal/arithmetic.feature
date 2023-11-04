Feature: arithmetic

  Scenario Outline: arithmetic
    When evaluate by:
    """
      <input> <operator> <value>
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | input | operator | value | result | inspect |
      | 10    | +        | 1     | 11     | 10 + 1  |
      | 10    | -        | 1     | 9      | 10 - 1  |
      | 10    | *        | 2     | 20     | 10 * 2  |
      | 10    | /        | 2     | 5      | 10 / 2  |

  Scenario Outline: unary opt
    When evaluate by:
    """
      (<input>)
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    (<inspect>)
    """
    Examples:
      | input | result | inspect |
      | -1    | -1     | -1      |
      | !true | false  | !true   |

#  Scenario: should not parse as minus when start with minus (old version DAL)
  Scenario: start with minus const number should parse as a negative const number
    Given the following json:
    """
    1
    """
    When evaluate by:
    """
    -1
    """
    Then the result should:
    """
#    old version DAL
#    : 0
    : -1
    """
