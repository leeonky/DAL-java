Feature: list node

  Scenario: return null when does not match
    Given the following dal code xx:
    """
    +
    """
    Then got the following "list" node xx:
    """
    : null
    """

  Scenario: support empty list
    Given the following dal code xx:
    """
     []
    """
    Then got the following "list" node xx:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[]'
      positionBegin: 1
    }
    """
