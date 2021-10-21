Feature: schema expression

  Scenario: return null when not start with key word is
    Given the following dal code:
    """
    +1
    """
    Then got the following "schema-expression" node:
    """
    : null
    """

  Scenario: support schema checking expression
    Given the following dal code:
    """
      is Integer
    """
    Then got the following "schema-expression" node:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: 'is Integer'
      positionBegin: 2
    }
    """
    When the following input data:
    """
    1
    """
    Then node evaluate result is:
    """
    = 1BI
    """

  Scenario: raise error when right operand is not schema
    Given the following dal code:
    """
    is +
    """
    Then failed to get "schema-expression" node with the following message:
    """
    expect a schema
    """
    And got the following source code information:
    """
    is +
       ^
    """

  Scenario: raise error when missing schema at the end of code
    Given the following dal code:
    """
    is
    """
    Then failed to get "schema-expression" node with the following message:
    """
    expect a schema
    """
    And got the following source code information:
    """
    is
      ^
    """

  Scenario: support multi schemas in expression
    Given the following dal code:
    """
    is Integer / Number
    """
    Then got the following "schema-expression" node:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: 'is Integer / Number'
    }
    """

  Scenario: raise error when schema list not finished
    Given the following dal code:
    """
    is Integer /
    """
    Then failed to get "schema-expression" node with the following message:
    """
    expect a schema
    """
    And got the following source code information:
    """
    is Integer /
                ^
    """

#  TODO
#  Scenario: support element schema expression
#    Given the following dal code:
#    """
#      is [Integer]
#    """
#    Then got the following "schema-expression" node:
#    """
#    : {
#      class.simpleName: 'SchemaExpression'
#      inspect: 'is [Integer]'
#      positionBegin: 2
#    }
#    """
#    When the following input data:
#    """
#    [1]
#    """
#    Then node evaluate result is:
#    """
#    = [1BI]
#    """
