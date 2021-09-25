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
