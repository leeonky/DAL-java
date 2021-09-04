Feature: schema which expression

  Scenario: support schema which expression
    Given the following dal code:
    """
      1 is Integer which true
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaWhichExpression'
      inspect: '1 is Integer which true'
      positionBegin: 4
    }
    """

  Scenario: complex which clause
    Given the following dal code:
    """
      1 is Integer which true and true
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaWhichExpression'
      inspect: '1 is Integer which true and true'
    }
    """

  Scenario: should raise error when no clause
    Given the following dal code:
    """
      1 is Integer which
    """
    Then failed to get "expression" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
      1 is Integer which
                        ^
    """
