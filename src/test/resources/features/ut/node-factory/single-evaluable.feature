Feature: single evaluable node

  Scenario: reference 'this' object when code is empty
    Given the following dal code:
    """

    """
    Then got the following "single-evaluable" node:
    """
    class.simpleName: 'InputNode'
    """

  Scenario Outline: supported single evaluable node
    Given the following dal code:
    """
      <code>
    """
    Then got the following "single-evaluable" node:
    """
    : {
      class.simpleName: '<type>'
      inspect: '<inspect>'
      positionBegin: <position>
    }
    """
    Examples:
      | code     | type            | inspect    | position |
      | 100      | ConstNode       | 100        | 2        |
      | (1+1)    | ParenthesesNode | (1 + 1)    | 2        |
      | .name    | PropertyNode    | .name      | 2        |
      | [0]      | PropertyNode    | [0]        | 2        |
      | "".empty | PropertyNode    | \'\'.empty | 4        |

  Scenario: recursive property node
    Given the following dal code:
    """
    .order.lines[0].name
    """
    Then got the following "single-evaluable" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '.order.lines[0].name'
      positionBegin: 15
    }
    """

  Scenario: should reset 'this' node
    Given the following dal code:
    """
    .a.c+.b
    """
    Then got the following "expression" node:
    """
    : {
      inspect: '.a.c + .b'
    }
    """

  Scenario: should raise error when not a evaluable node
    Given the following dal code:
    """
    1 +
    """
    And get an "single-evaluable" node
    Then failed to get "single-evaluable" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    1 +
      ^
    """

  Scenario: should raise error when at the end of token
    Given the following dal code:
    """
    1
    """
    And get an "single-evaluable" node
    Then failed to get "single-evaluable" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    1
     ^
    """

  Scenario: support minus before node
    Given the following dal code:
    """
    1 -1
    """
    And get an "const" node
    Then got the following "single-evaluable" node:
    """
    class.simpleName: 'Expression'
    inspect: '-1'
    """

  Scenario: should not parse as minus when start with minus
    Given the following input data:
    """
    2
    """
    And the following dal code:
    """
    -1
    """
    Then got the following "single-evaluable" node:
    """
    : {
      class.simpleName: 'InputNode'
      inspect: ''
    }
    """

  Scenario: support logic not
    Given the following dal code:
    """
    !false
    """
    Then got the following "single-evaluable" node:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '!false'
    }
    """
    And evaluate result is:
    """
    =true
    """
