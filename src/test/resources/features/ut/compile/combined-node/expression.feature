Feature: expression

  Scenario: binary operator expression
    Given the following dal code xx:
    """
     + b
    """
    Then got the following "expression" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ b'
    }
    """

  Scenario: schema expression
    Given the following dal code xx:
    """
     is Number
    """
    Then got the following "expression" node xx:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: 'is Number'
    }
    """

  Scenario: operator expression after schema expression
    Given the following dal code xx:
    """
    is Integer || true
    """
    Then got the following "expression" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: 'is Integer || true'
    }
    """

  Scenario: schema expression after operator expression
    Given the following dal code xx:
    """
    + 1 is Number
    """
    Then got the following "expression" node xx:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: '+ 1 is Number'
    }
    """

  Scenario: support expression chain and process the operator precedence
    Given the following dal code xx:
    """
    + 2 * 3
    """
    Then got the following "expression" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ 2 * 3'
      positionBegin: 0
      leftOperand.class.simpleName: 'InputNode'
      operator.class.simpleName: 'Plus'
      rightOperand.inspect: '2 * 3'
      rightOperand.positionBegin: 4
    }
    """
