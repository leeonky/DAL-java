Feature: list node

  Scenario: return null when does not match
    Given the following dal code:
    """
    +
    """
    Then got the following "list" node:
    """
    : null
    """

  Scenario: support empty list
    Given the following dal code:
    """
     []
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[]'
      positionBegin: 1
    }
    """

  Scenario: support one element list
    Given the following dal code:
    """
     [1]
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[1]'
      positionBegin: 1
    }
    """

  Scenario: support two elements list
    Given the following dal code:
    """
     [1 2]
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[1 2]'
      positionBegin: 1
    }
    """

  Scenario: raise error when no closing bracket
    Given the following dal code:
    """
    [
    """
    Then failed to get "list" node with the following message:
    """
    missed `]`
    """
    And got the following source code information:
    """
    [
     ^
    """

  Scenario: raise error when element is invalid
    Given the following dal code:
    """
    [ + ]
    """
    Then failed to get "list" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    [ + ]
      ^
    """

#  TODO support comma
#  TODO nested list
#  TODO nested object
#  TODO support comma
