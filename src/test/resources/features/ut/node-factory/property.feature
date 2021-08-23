Feature: property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "property" node:
    """
    : null
    """

  Scenario: compile as property node
    Given the following dal code:
    """
      .name
    """
    Then got the following "property" node:
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
    Then got the following "property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[100]'
    }
    """

  Scenario: support compile property node with out dot operator
    Given the following dal code:
    """
      name
    """
    Then got the following "property" node:
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
    Then got the following "property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 7
      inspect: 'user.name'
    }
    """

  Scenario: evaluate property node
    Given the following input data:
    """
      {
        "user": {
          "name": "Tom"
        }
      }
    """
    Then the following assertion should be passed:
    """
    user.name = 'Tom'
    """

#TODO    field alias