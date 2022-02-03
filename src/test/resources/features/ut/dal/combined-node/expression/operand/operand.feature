Feature: operand node

  Scenario: reference 'this' object when code is empty
    Given the following dal code:
    """
      
    """
    Then got the following "operand" node:
    """
    class.simpleName: 'InputNode'
    """
    When the following input data:
    """
      1
    """
    Then node evaluate result is:
    """
    : 1
    """

  Scenario Outline: supported single evaluable node
    Given the following dal code:
    """
      <code>
    """
    Then got the following "operand" node:
    """
    : {
      class.simpleName: '<type>'
      inspect: '<inspect>'
    }
    """
    Examples:
      | code  | type            | inspect |
      | 100   | ConstNode       | 100     |
      | name  | PropertyNode    | name    |
      | .name | PropertyNode    | .name   |
      | [0]   | PropertyNode    | [0]     |
      | (1+1) | ParenthesesNode | (1 + 1) |

  Scenario: recursive property node
    Given the following dal code:
    """
    .order.lines[0].name
    """
    Then got the following "operand" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '.order.lines[0].name'
      positionBegin: 15
    }
    """
    When the following input data:
    """
      { "order": { "lines": [{ "name": "book" }] } }
    """
    Then node evaluate result is:
    """
    : 'book'
    """

  Scenario: support minus before node
    Given the following dal code:
    """
    1 -1
    """
    And ignore an "const" node
    Then got the following "operand" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: '-1'
      positionBegin: 2
    }
    """
    Then node evaluate result is:
    """
    : -1
    """

  Scenario: should not parse as minus when start with minus
    Given the following dal code:
    """
    -1
    """
    Then got the following "operand" node:
    """
    : {
      class.simpleName: 'InputNode'
      inspect: ''
    }
    """

  Scenario: support logic not
    Given the following dal code:
    """
    !false
    """
    Then got the following "operand" node:
    """
    : {
      leftOperand: null
      class.simpleName: 'DALExpression'
      inspect: '!false'
    }
    """
    And node evaluate result is:
    """
    =true
    """

  Scenario: logic not and not equal
    Given the following dal code:
    """
    !=1
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: '!= 1'
      leftOperand.class.simpleName: 'InputNode'
    }
    """

  Scenario: should raise error when not a evaluable node
    Given the following dal code:
    """
    1 +
    """
    And ignore an "const" node
    Then failed to get "operand" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    1 +
      ^
    """

  Scenario: should raise error when at the end of token
    Given the following dal code:
    """
    1
    """
    And ignore an "const" node
    Then failed to get "operand" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    1
     ^
    """


  Scenario: end with .@ is invalid
    Given the following dal code:
    """
      .@
    """
    Then failed to get "operand" node with the following message:
    """
    element property needed
    """
    And got the following source code information:
    """
      .@
      ^
    """
