Feature: bracket property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "bracket-property" node:
    """
    : null
    """

  Scenario: compile access list as property node
    Given the following dal code:
    """
      [100]
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[100]'
    }
    """
