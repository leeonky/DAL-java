Feature: right operand node

  Scenario: must take a right operand node
    Given the following dal code:
    """
    is Object which
    """
    Then failed to get "expression" node with the following message:
    """
    expect a value or expression
    """

  Scenario: regex node
    Given the following dal code:
    """
    : /hello/
    """
    Then got the following "expression" node:
    """
    rightOperand.class.simpleName: 'RegexNode'
    """

  Scenario: object node
    Given the following dal code:
    """
    : {}
    """
    Then got the following "expression" node:
    """
    rightOperand.class.simpleName: 'ObjectNode'
    """

  Scenario: list node
    Given the following dal code:
    """
    : []
    """
    Then got the following "expression" node:
    """
    rightOperand.class.simpleName: 'ListNode'
    """

  Scenario Outline: supported single evaluable node
    Given the following dal code:
    """
    <code>
    """
    Then got the following "right-operand" node:
    """
    class.simpleName: '<type>'
    """
    Examples:
      | code     | type            |
      | 100      | ConstNode       |
      | (1+1)    | ParenthesesNode |
      | .name    | PropertyNode    |
      | "".empty | PropertyNode    |
