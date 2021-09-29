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

  Scenario: support one judgement expression
    Given the following dal code xx:
    """
    { name = 'Tom' }
    """
    Then got the following "object" node xx:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: "{name = 'Tom'}"
    }
    """

  Scenario: support two judgement expressions
    Given the following dal code xx:
    """
    {
      name = 'Tom'
      age = 30
    }
    """
    Then got the following "object" node xx:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: "{name = 'Tom' age = 30}"
    }
    """

  Scenario: raise error when no closing brace
    Given the following dal code xx:
    """
    {
    """
    Then failed to get "object" node with the following message xx:
    """
    should end with `}`
    """
    And got the following source code information xx:
    """
    {
     ^
    """

  Scenario: raise error when element is invalid
    Given the following dal code xx:
    """
    { name: + }
    """
    Then failed to get "object" node with the following message xx:
    """
    expect a value or expression
    """
    And got the following source code information xx:
    """
    { name: + }
            ^
    """

  Scenario: raise error when invalid judgement expression
    Given the following dal code xx:
    """
    { 1: 1 }
    """
    Then failed to get "object" node with the following message xx:
    """
    expect a object property
    """
    And got the following source code information xx:
    """
    { 1: 1 }
      ^
    """

  Scenario: raise error when not judgement expression
    Given the following dal code xx:
    """
    { a + 1 }
    """
    Then failed to get "object" node with the following message xx:
    """
    expect operator `:` or `=`
    """
    And got the following source code information xx:
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

  Scenario: support optional comma between after sub expression
    Given the following dal code:
    """
     {
       key1: 1,
       key2: 2
     }
    """
    Then got the following "object" node:
    """
    inspect: '{key1 : 1 key2 : 2}'
    """
