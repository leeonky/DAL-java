Feature: compile judgement expression operand

  Scenario: return 'this' object when no code
    Given the following dal code xx:
    """
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    : {
      class.simpleName: 'InputNode'
      inspect: ''
    }
    """

  Scenario: return left operand when no right operand
    Given the following dal code xx:
    """
      1
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '1'
    }
    """

  Scenario: default use 'this' object as left operand
    Given the following dal code xx:
    """
      + 1
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ 1'
      positionBegin: 2
    }
    """

  Scenario: raise error when expression is not finished
    Given the following dal code xx:
    """
      1+
    """
    Then failed to get "judgement-expression-operand" node with the following message xx:
    """
    expect a value or expression
    """
    And got the following source code information xx:
    """
      1+
        ^
    """

  Scenario Outline: supported operator expressions
    Given the following dal code xx:
    """
    a <operator> b
    """
    Then got the following "judgement-expression-operand" node xx:
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
    Given the following dal code xx:
    """
      1<operator>
    """
    Then got the following "judgement-expression-operand" node xx:
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
    Given the following dal code xx:
    """
    a + b * c
    """
    Then got the following "judgement-expression-operand" node xx:
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

  Scenario: compile right operand as element accessing when operator is not judgement
    Given the following dal code xx:
    """
      + [0]
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ [0]'
      rightOperand.class.simpleName: 'PropertyNode'
    }
    """

  Scenario: regex node
    Given the following dal code xx:
    """
    /hello/
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    class.simpleName: 'RegexNode'
    """

  Scenario: object node
    Given the following dal code xx:
    """
    {}
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    class.simpleName: 'ObjectNode'
    """

  Scenario: list node
    Given the following dal code xx:
    """
    []
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    class.simpleName: 'ListNode'
    """

  Scenario: wildcard
    Given the following dal code xx:
    """
    *
    """
    Then got the following "judgement-expression-operand" node xx:
    """
    class.simpleName: 'WildcardNode'
    """
