Feature: object node

  Scenario: return null when does not match
    Given the following dal code xx:
    """
    +
    """
    Then got the following "object" node xx:
    """
    : null
    """

  Scenario: support empty object with no field
    Given the following dal code xx:
    """
      {}
    """
    Then got the following "object" node xx:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: '{}'
      positionBegin: 2
    }
    """
