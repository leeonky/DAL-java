Feature: expression

  Scenario: return 'this' object when no code
    Given the following dal code:
    """
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'InputNode'
      inspect: ''
    }
    """

  Scenario: single const node
    Given the following dal code:
    """
      1
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '1'
    }
    """

  Scenario Outline: property node
    Given the following dal code:
    """
      <code>
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '<code>'
    }
    """
    Examples:
      | code |
      | a    |
      | .a   |
      | .a.b |

  Scenario: default use 'this' object as left operand
    Given the following dal code:
    """
      + 1
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: '+ 1'
      positionBegin: 2
    }
    """

  Scenario: binary operator expression
    Given the following dal code:
    """
    a + b
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: 'a + b'
    }
    """

  Scenario: schema expression
    Given the following dal code:
    """
     is Number
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: 'is Number'
    }
    """

  Scenario: operator expression after schema expression
    Given the following dal code:
    """
    is Integer || true
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: 'is Integer || true'
    }
    """

  Scenario: schema expression after operator expression
    Given the following dal code:
    """
    + 1 is Number
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: '+ 1 is Number'
    }
    """

  Scenario: support expression chain and process the operator precedence
    Given the following dal code:
    """
    + 2 * 3 * 4
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: '+ 2 * 3 * 4'
      positionBegin: 0
      leftOperand.class.simpleName: 'InputNode'
      operator.class.simpleName: 'Plus'
      rightOperand.inspect: '2 * 3 * 4'
      rightOperand.positionBegin: 8
    }
    """
