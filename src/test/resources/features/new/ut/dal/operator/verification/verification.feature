Feature: verification

  Scenario Outline: verification
    When evaluate by:
    """
      <input> <operator> <value>
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input | operator | value | result |
      | 5     | =        | 5     | true   |
      | 3     | :        | 3.0   | true   |

