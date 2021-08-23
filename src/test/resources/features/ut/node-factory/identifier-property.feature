Feature: identifier property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "identifier-property" node:
    """
    : null
    """

  Scenario: support compile property node with out dot operator
    Given the following dal code:
    """
      name
    """
    Then got the following "identifier-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: 'name'
    }
    """

  Scenario: support compile property chain node with out dot operator
    Given the following dal code:
    """
      user.name
    """
    Then got the following "identifier-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 7
      inspect: 'user.name'
    }
    """
