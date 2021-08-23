Feature: object node

  Scenario: return null when does not match
    Given the following dal code:
    """
    +
    """
    Then got the following "object" node:
    """
    : null
    """

  Scenario: support empty object with no field
    Given the following dal code:
    """
    {}
    """
    Then got the following "object" node:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: '{}'
    }
    """

  Scenario: support one judgement expression
    Given the following dal code:
    """
    { name = 'Tom' }
    """
    Then got the following "object" node:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: "{name = 'Tom'}"
    }
    """

  Scenario: support two judgement expressions
    Given the following dal code:
    """
    {
      name = 'Tom'
      age = 30
    }
    """
    Then got the following "object" node:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: "{name = 'Tom' age = 30}"
    }
    """

  Scenario: raise error when no closing brace
    Given the following dal code:
    """
    {
    """
    Then failed to get "object" node with the following message:
    """
    missed `}`
    """
    And got the following source code information:
    """
    {
     ^
    """

#  TODO support comma
#  TODO nested list
#  TODO nested object
#  TODO support comma
