Feature: not

  Scenario Outline: logic not operator
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code   | value |
      | !false | true  |
      | !true  | false |
