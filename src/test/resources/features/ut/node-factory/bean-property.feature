Feature: bean property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "bean-property" node:
    """
    : null
    """

  Scenario: compile as property node
    Given the following dal code:
    """
      .name
    """
    Then got the following "bean-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '.name'
    }
    """