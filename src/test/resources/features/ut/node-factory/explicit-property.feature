Feature: explicit property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "explicit-property" node:
    """
    : null
    """

  Scenario: compile as property node
    Given the following dal code:
    """
      .name
    """
    Then got the following "explicit-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '.name'
    }
    """

  Scenario: compile access list as property node
    Given the following dal code:
    """
      [100]
    """
    Then got the following "explicit-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[100]'
    }
    """
