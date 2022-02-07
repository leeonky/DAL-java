Feature: compile key word const value (true false null)

  Scenario Outline: key word const
    When evaluate by:
    """
     <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code  | value |
      | true  | true  |
      | false | false |
      | null  | null  |

  Scenario: key word const position
    When evaluate by:
    """
    null= true
    """
    Then got the following notation:
    """
    null= true
          ^
    """
