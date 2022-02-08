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
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code  | value | inspect |
      | true  | true  | true    |
      | false | false | false   |
      | null  | null  | null    |

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
