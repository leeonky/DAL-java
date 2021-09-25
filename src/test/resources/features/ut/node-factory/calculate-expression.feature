Feature: calculator expression

  Scenario: return 'this' object when no code
    Given the following dal code:
    """
    """
    Then got the following "calculator-expression" node:
    """
    : {
      class.simpleName: 'InputNode'
      inspect: ''
    }
    """

  Scenario: return left operand when no right operand
    Given the following dal code:
    """
      1
    """
    Then got the following "calculator-expression" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '1'
    }
    """

  Scenario: default use 'this' object as left operand
    Given the following dal code:
    """
      + 1
    """
    Then got the following "calculator-expression" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ 1'
      positionBegin: 2
    }
    """

  Scenario: raise error when mission (
    Given the following dal code:
    """
      1)
    """
    Then failed to get "calculator-expression" node with the following message:
    """
    missed '('
    """
    And got the following source code information:
    """
      1)
       ^
    """

  Scenario: raise error when expression is not finished
    Given the following dal code:
    """
      1+
    """
    Then failed to get "calculator-expression" node with the following message:
    """
    expression is not finished
    """
    And got the following source code information:
    """
      1+
        ^
    """

  Scenario Outline: supported operator expressions
    Given the following dal code:
    """
    a <operator> b
    """
    Then got the following "calculator-expression" node:
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
    Then got the following "calculator-expression" node:
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
    Then got the following "calculator-expression" node:
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
    Given the following dal code:
    """
      + [0]
    """
    Then got the following "calculator-expression" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ [0]'
      rightOperand.class.simpleName: 'PropertyNode'
    }
    """
