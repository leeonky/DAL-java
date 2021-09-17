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

  Scenario: compile .@ chain to one property node
    Given the following dal code:
    """
      .@.x
    """
    Then got the following "bean-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '.@.x'
    }
    """

  Scenario: end with .@ is invalid
    Given the following dal code:
    """
      .@
    """
    Then failed to get "bean-property" node with the following message:
    """
    element property needed
    """
    And got the following source code information:
    """
      .@
        ^
    """
