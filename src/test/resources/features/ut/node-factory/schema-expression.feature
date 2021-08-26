Feature: schema expression

  Scenario: support schema checking expression
    Given the following dal code:
    """
      1 is Integer
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: '1 is Integer'
      positionBegin: 2
    }
    """

  Scenario: raise error when right operand is not schema
    Given the following dal code:
    """
    1 is 1
    """
    Then failed to get "expression" node with the following message:
    """
    operand of `is` must be schema type
    """
    And got the following source code information:
    """
    1 is 1
         ^
    """

  Scenario: raise error when missing schema at the end of code
    Given the following dal code:
    """
    1 is
    """
    Then failed to get "expression" node with the following message:
    """
    schema expression not finished
    """
    And got the following source code information:
    """
    1 is
        ^
    """

  Scenario: support multi schemas in expression
    Given the following dal code:
    """
    1 is Integer | Number
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: '1 is Integer | Number'
    }
    """

  Scenario: raise error when schema operator is different
    Given the following dal code:
    """
    1 is Integer | Number / Long
    """
    Then failed to get "expression" node with the following message:
    """
    schema operator was different
    """
    And got the following source code information:
    """
    1 is Integer | Number / Long
                            ^
    """

  Scenario: raise error when schema list not finished
    Given the following dal code:
    """
    1 is Integer |
    """
    Then failed to get "expression" node with the following message:
    """
    schema expression not finished
    """
    And got the following source code information:
    """
    1 is Integer |
                  ^
    """
