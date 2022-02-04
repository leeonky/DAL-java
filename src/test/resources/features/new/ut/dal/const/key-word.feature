Feature: compile key word const value (true false null)

  Scenario Outline: key word const
    Given the following dal expression:
    """
     <code>
    """
    Then parse the following "expression" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '<inspect>'
      positionBegin: 1
    }
    """
    And last evaluated node result is:
    """
    = <value>
    """
    Examples:
      | code  | inspect | value |
      | true  | true    | true  |
      | false | false   | false |
      | null  | null    | null  |
