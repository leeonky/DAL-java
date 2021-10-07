Feature: binary operator expression

  Scenario: return null when not start with arithmetic operator
    Given the following dal code:
    """
    1
    """
    Then got the following "binary-operator-expression" node:
    """
    : null
    """

  Scenario Outline: supported arithmetic operators
    Given the following dal code:
    """
     <operator> b
    """
    Then got the following "binary-operator-expression" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '<operator> b'
      positionBegin: 1
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
      | !=       | NotEqual       |
      | =        | Equal          |
      | :        | Matcher        |

  Scenario Outline: judge by object list or regex
    Given the following dal code:
    """
    <operator> <operand>
    """
    Then got the following "binary-operator-expression" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '<operator> <operand>'
    }
    """
    Examples:
      | operator | operand   |
      | =        | {}        |
      | =        | []        |
      | =        | /pattern/ |
      | :        | {}        |
      | :        | []        |
      | :        | /pattern/ |

  Scenario: raise error when expression is not finished
    Given the following dal code:
    """
      +
    """
    Then failed to get "binary-operator-expression" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
      +
       ^
    """

  Scenario: compile right operand as element accessing when operator is not judgement
    Given the following dal code:
    """
      + [0]
    """
    Then got the following "binary-operator-expression" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ [0]'
      rightOperand.class.simpleName: 'PropertyNode'
    }
    """

  Scenario Outline: force positive judgement
    Given the following dal code:
    """
    <operator> *
    """
    Then got the following "binary-operator-expression" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '<operator> *'
    }
    """
    And node evaluate result is:
    """
    :true
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: operator of judgement list
    Given the following dal code:
    """
    <operator> [1 2]
    """
    Then got the following "binary-operator-expression" node:
    """
    rightOperand.expressions.inspect: [
      '[0] <operator> 1'
      '[1] <operator> 2'
    ]
    """
    Examples:
      | operator |
      | =        |
      | :        |
