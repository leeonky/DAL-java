Feature: expression

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
      class.simpleName: 'DALExpression'
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
