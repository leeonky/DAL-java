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
      positionBegin: 2
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

  Scenario: raise error when element is invalid
    Given the following dal code:
    """
    { name: + }
    """
    Then failed to get "object" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    { name: + }
            ^
    """

  Scenario: raise error when invalid judgement expression
    Given the following dal code:
    """
    { 1: 1 }
    """
    Then failed to get "object" node with the following message:
    """
    expect a object property
    """
    And got the following source code information:
    """
    { 1: 1 }
      ^
    """

  Scenario: raise error when not judgement expression
    Given the following dal code:
    """
    { a + 1 }
    """
    Then failed to get "object" node with the following message:
    """
    expect operator `:` or `=`
    """
    And got the following source code information:
    """
    { a + 1 }
        ^
    """

  Scenario: raise error when missing judgement operator
    Given the following dal code:
    """
    { a 1 }
    """
    Then failed to get "object" node with the following message:
    """
    expect operator `:` or `=`
    """
    And got the following source code information:
    """
    { a 1 }
        ^
    """

#  TODO support comma
#  TODO nested list
#  TODO nested object
#  TODO support comma
