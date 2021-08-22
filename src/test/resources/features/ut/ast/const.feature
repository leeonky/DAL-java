Feature: compile to const node

  Scenario: return null when does not match
    Given the follow dal code:
    """
    +
    """
    Then got the following "const" node:
    """
    : null
    """

  Scenario: compile to const node
    Given the follow dal code:
    """
      100
    """
    Then got the following "const" node:
    """
    : {
      class.simpleName: 'ConstNode'
      positionBegin: 2
      inspect: '100'
    }
    """
    And evaluate result is:
    """
    : 100
    """
