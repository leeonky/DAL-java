Feature: schema which expression

  Scenario: support schema which expression
    Given the following dal code:
    """
      is Integer which true and true
    """
    Then got the following "schema-expression" node:
    """
    : {
      class.simpleName: 'SchemaWhichExpression'
      inspect: 'is Integer which true and true'
      positionBegin: 2
    }
    """

  Scenario: should raise error when no clause
    Given the following dal code:
    """
      is Integer which
    """
    Then failed to get "schema-expression" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
      is Integer which
                      ^
    """

  Scenario Outline: judgement operator after which
    Given the following dal code:
    """
      is Integer which <operator> 1
    """
    Then got the following "schema-expression" node:
    """
    : {
      class.simpleName: 'SchemaWhichExpression'
      inspect: 'is Integer which <operator> 1'
    }
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: can omit which when clause start with = or :
    Given the following dal code:
    """
      is Integer <operator> 1
    """
    Then got the following "expression" node:
    """
    : {
      class.simpleName: 'SchemaWhichExpression'
      inspect: 'is Integer <operator> 1'
    }
    """
    Examples:
      | operator |
      | :        |
      | =        |
