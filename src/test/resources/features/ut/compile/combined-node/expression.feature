Feature: expression

  Scenario: binary operator expression
    Given the following dal code xx:
    """
     + b
    """
    Then got the following "binary-operator-expression" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '+ b'
    }
    """
