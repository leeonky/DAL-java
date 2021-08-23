Feature: const node

  Scenario: return null when does not match
    Given the following dal code:
    """
    +
    """
    Then got the following "const" node:
    """
    : null
    """

  Scenario Outline: compile to const node
    Given the following dal code:
    """
      <code>
    """
    Then got the following "const" node:
    """
    : {
      class.simpleName: 'ConstNode'
      positionBegin: 2
      inspect: '<inspect>'
    }
    """
    And evaluate result is:
    """
    : <evaluate>
    """
    Examples:
      | code  | inspect | evaluate |
      | 100   | 100     | 100      |
      | null  | null    | null     |
      | 'str' | \'str\' | 'str'    |
