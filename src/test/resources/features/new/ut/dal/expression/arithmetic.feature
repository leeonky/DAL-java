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
