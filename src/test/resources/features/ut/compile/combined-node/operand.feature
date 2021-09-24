Feature: operand node

  Scenario: reference 'this' object when code is empty
    Given the following dal code xx:
    """
      
    """
    Then got the following "operand" node xx:
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
    Then got the following "operand" node xx:
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

  Scenario: recursive property node
    Given the following dal code xx:
    """
    .order.lines[0].name
    """
    Then got the following "operand" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '.order.lines[0].name'
      positionBegin: 15
    }
    """
    When the following input data xx:
    """
      { "order": { "lines": [{ "name": "book" }] } }
    """
    Then evaluate result is xx:
    """
    : 'book'
    """

  Scenario: support minus before node
    Given the following dal code xx:
    """
    1 -1
    """
    And ignore an "const" node xx
    Then got the following "operand" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '-1'
      positionBegin: 2
    }
    """
    Then evaluate result is xx:
    """
    : -1
    """

  Scenario: should not parse as minus when start with minus
    Given the following dal code xx:
    """
    -1
    """
    Then got the following "operand" node xx:
    """
    : {
      class.simpleName: 'InputNode'
      inspect: ''
    }
    """

  Scenario: support logic not
    Given the following dal code xx:
    """
    !false
    """
    Then got the following "operand" node xx:
    """
    : {
      class.simpleName: 'Expression'
      inspect: '!false'
    }
    """
    And evaluate result is xx:
    """
    =true
    """

  Scenario: should raise error when not a evaluable node
    Given the following dal code xx:
    """
    1 +
    """
    And ignore an "const" node xx
    Then failed to get "operand" node with the following message xx:
    """
    expect a value or expression
    """
    And got the following source code information xx:
    """
    1 +
      ^
    """

  Scenario: should raise error when at the end of token
    Given the following dal code xx:
    """
    1
    """
    And ignore an "const" node xx
    Then failed to get "operand" node with the following message xx:
    """
    expect a value or expression
    """
    And got the following source code information xx:
    """
    1
     ^
    """
