Feature: binary operator expression

  Scenario: return null when not start with arithmetic operator
    Given the following dal code xx:
    """
    1
    """
    Then got the following "binary-operator-expression" node xx:
    """
    : null
    """

  Scenario Outline: supported arithmetic operators
    Given the following dal code xx:
    """
     <operator> b
    """
    Then got the following "binary-operator-expression" node xx:
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
      | =        | Equal          |
      | :        | Matcher        |

  Scenario Outline: judge by object list or regex
    Given the following dal code xx:
    """
    <operator> <operand>
    """
    Then got the following "binary-operator-expression" node xx:
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
    Given the following dal code xx:
    """
      +
    """
    Then failed to get "binary-operator-expression" node with the following message xx:
    """
    expect a value or expression
    """
    And got the following source code information xx:
    """
      +
       ^
    """

  Scenario: compile right operand as element accessing when operator is not judgement
    Given the following dal code xx:
    """
      + [0]
    """
    Then got the following "binary-operator-expression" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ [0]'
      rightOperand.class.simpleName: 'PropertyNode'
    }
    """

  Scenario Outline: force positive judgement
    Given the following dal code xx:
    """
    <operator> *
    """
    Then got the following "expression" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '<operator> *'
    }
    """
    And evaluate result is xx:
    """
    :true
    """
    Examples:
      | operator |
      | =        |
      | :        |
