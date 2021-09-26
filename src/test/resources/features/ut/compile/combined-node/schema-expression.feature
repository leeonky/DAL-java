Feature: schema expression

  Scenario: return null when not start with key word is
    Given the following dal code xx:
    """
    +1
    """
    Then got the following "schema-expression" node xx:
    """
    : null
    """

  Scenario: support schema checking expression
    Given the following dal code xx:
    """
      is Integer
    """
    Then got the following "schema-expression" node xx:
    """
    : {
      class.simpleName: 'SchemaExpression'
      inspect: 'is Integer'
      positionBegin: 2
    }
    """

  Scenario: raise error when right operand is not schema
    Given the following dal code xx:
    """
    is +
    """
    Then failed to get "schema-expression" node with the following message xx:
    """
    operand of `is` must be schema type
    """
    And got the following source code information xx:
    """
    is +
       ^
    """

  Scenario: raise error when missing schema at the end of code
    Given the following dal code xx:
    """
    is
    """
    Then failed to get "schema-expression" node with the following message xx:
    """
    schema expression not finished
    """
    And got the following source code information xx:
    """
    is
      ^
    """
