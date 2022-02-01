Feature: compile all const value node

  Scenario Outline: compile const node
    Given the following dal code:
    """
     <code>
    """
    Then got the following "const" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: <inspect>
      positionBegin: 1
    }
    """
    Examples:
      | code  | inspect |
      | 100   | '100'   |
      | 'a'   | "'a'"   |
      | "a"   | "'a'"   |
      | true  | 'true'  |
      | false | 'false' |
      | null  | 'null'  |
