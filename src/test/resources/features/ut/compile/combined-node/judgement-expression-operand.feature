Feature: compile judgement expression operand

  Scenario: single const node
    Given the following dal code:
    """
      1
    """
    Then got the following "judgement-expression-operand" node:
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
    Then got the following "judgement-expression-operand" node:
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

  Scenario Outline: operator expressions
    Given the following dal code:
    """
    a <operator> b
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: 'a <operator> b'
      positionBegin: 2
      leftOperand.inspect: 'a'
      operator.class.simpleName: '<type>'
      rightOperand.inspect: 'b'
    }
    """
    Examples:
      | operator | type           |
      | +        | Plus           |
      | -        | Subtraction    |
      | *        | Multiplication |
      | /        | Division       |
      | &&       | And            |
      | \|\|     | Or             |
      | and      | And            |
      | or       | Or             |
      | >        | Greater        |
      | <        | Less           |
      | >=       | GreaterOrEqual |
      | <=       | LessOrEqual    |

  Scenario Outline: should finish expression when got judgement operator
    Given the following dal code:
    """
      1<operator>
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '1'
    }
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario: support expression chain and process the operator precedence
    Given the following dal code:
    """
    a + b * c
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: 'a + b * c'
      positionBegin: 2
      leftOperand.inspect: 'a'
      operator.class.simpleName: 'Plus'
      rightOperand.inspect: 'b * c'
      rightOperand.positionBegin: 6
    }
    """

  Scenario: compile as element accessing when operator is not judgement
    Given the following dal code:
    """
    a + [0]
    """
    Then got the following "judgement-expression-operand" node:
    """
    rightOperand.class.simpleName: 'PropertyNode'
    """

  Scenario: regex node
    Given the following dal code:
    """
    /hello/
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'RegexNode'
    """

  Scenario: object node
    Given the following dal code:
    """
    {}
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'ObjectNode'
    """

  Scenario: list node
    Given the following dal code:
    """
    []
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'ListNode'
    """

  Scenario: parentheses
    Given the following dal code:
    """
    (1)
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'ParenthesesNode'
      inspect: '(1)'
    }
    """

  Scenario: wildcard
    Given the following dal code:
    """
    *
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'WildcardNode'
    """

  Scenario: raise error when expression is not finished
    Given the following dal code:
    """
      1+
    """
    Then failed to get "judgement-expression-operand" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
      1+
        ^
    """

  Scenario: raise error when no code
    Given the following dal code:
    """
    a
    """
    And ignore an "property" node
    Then failed to get "judgement-expression-operand" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    a
     ^
    """

  Scenario Outline: operator of judgement list
    Given the following dal code:
    """
    {
      a<operator> [1 2]
    }
    """
    Then got the following "object" node:
    """
    expressions[0].rightOperand.expressions.inspect: [
      '[0]<operator> 1'
      '[1]<operator> 2'
    ]
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario: support table node
    Given the following dal code:
    """
    | name: | age= |
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'TableNode'
    """
