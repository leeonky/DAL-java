Feature: compile judgement expression operand

  Scenario: wildcard
    Given the following dal code:
    """
    *
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'WildcardNode'
    """

  Scenario: support table node
    Given the following dal code:
    """
    | name: | age= |
    """
    Then got the following "judgement-expression-operand" node:
    """
    class.simpleName: 'TableNode'
    """
