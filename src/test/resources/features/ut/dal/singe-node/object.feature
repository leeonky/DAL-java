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
      class.simpleName: 'ObjectScopeNode'
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
      class.simpleName: 'ObjectScopeNode'
      inspect: "{name= 'Tom'}"
    }
    """

  Scenario: support property chain key
    Given the following dal code:
    """
    { user.name.first = 'Tom' }
    """
    Then got the following "object" node:
    """
    inspect: "{user.name.first= 'Tom'}"
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
      class.simpleName: 'ObjectScopeNode'
      inspect: "{name= 'Tom', age= 30}"
    }
    """

  Scenario: raise error when no closing brace
    Given the following dal code:
    """
    {
    """
    Then failed to get "object" node with the following message:
    """
    should end with `}`
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
    inspect: '{key1: 1, key2: 2}'
    """

  Scenario: support schema expression
    Given the following dal code:
    """
     {
       key1 is Schema
     }
    """
    Then got the following "object" node:
    """
    : {
      inspect: '{key1 is Schema}'
      expressions[0]: {
        class.simpleName: 'DALExpression'
        inspect: 'key1 is Schema'
      }
    }
    """

  Scenario: support schema in judgement expression
    Given the following dal code:
    """
     {
       key1 is String: 'hello'
     }
    """
    Then got the following "object" node:
    """
    : {
      inspect: "{key1 is String: 'hello'}"
      expressions[0]: {
        class.simpleName: 'DALExpression'
        leftOperand: {
          class.simpleName: 'DALExpression'
          inspect: 'key1 is String'
        }
        operator.class.simpleName: 'Matcher'
        rightOperand: {
          class.simpleName: 'ConstNode'
          inspect: "'hello'"
        }
      }
    }
    """
