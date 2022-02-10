Feature: binary operator expression

  Scenario Outline: judge by object list or regex
    Given the following dal code:
    """
    <operator> <operand>
    """
    Then got the following "binary-operator-expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
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

  Scenario Outline: force positive judgement
    Given the following dal code:
    """
    <operator> *
    """
    Then got the following "binary-operator-expression" node:
    """
    : {
      class.simpleName: 'DALExpression'
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
