Feature: left operand node

  Scenario: reference 'this' object when code is empty
    Given the following dal code xx:
    """
    """
    Then got the following "left-operand" node xx:
    """
    class.simpleName: 'InputNode'
    """
    When the following input data xx:
    """
      1
    """
    Then evaluate result is xx:
    """
    : 1
    """

  Scenario Outline: supported single evaluable node
    Given the following dal code xx:
    """
      <code>
    """
    Then got the following "left-operand" node xx:
    """
    : {
      class.simpleName: '<type>'
      inspect: '<inspect>'
    }
    """
    Examples:
      | code  | type         | inspect |
      | 100   | ConstNode    | 100     |
      | .name | PropertyNode | .name   |
      | [0]   | PropertyNode | [0]     |
