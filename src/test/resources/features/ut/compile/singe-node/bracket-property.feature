Feature: bracket property node

  Scenario: return null when does not match
    Given the following dal code xx:
    """
    not [
    """
    Then got the following "bracket-property" node xx:
    """
    : null
    """

  Scenario: access list element
    Given the following dal code xx:
    """
      [1]
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[1]'
    }
    """
    When the following input data xx:
    """
      [0, 1]
    """
    Then evaluate result is xx:
    """
    : 1
    """

  Scenario: access object property
    Given the following dal code xx:
    """
      ['first name']
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: "['first name']"
    }
    """
    When the following input data xx:
    """
      { "first name": "Tom" }
    """
    Then evaluate result is xx:
    """
    : 'Tom'
    """

  Scenario: access from the end of list
    Given the following dal code xx:
    """
    [-1]
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[-1]'
    }
    """
    When the following input data xx:
    """
      [0, 1]
    """
    Then evaluate result is xx:
    """
    : 1
    """
