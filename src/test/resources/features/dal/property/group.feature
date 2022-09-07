Feature: group

  Scenario Outline: support group with << >> and verification with same value
    Given the following json:
    """
    {
      "a": 1,
      "b": 1,
      "c": {
        "value": 2
      }
    }
    """
    Then the following verification should pass:
    """
    <<a b>><opt> 1
    """
    And the inspect should:
    """
    <<a, b>><opt> 1
    """
    And evaluate by:
    """
    <<a, c.value>><opt> 1
    """
    Then failed with the message:
    """
    Expected to <text>: java.lang.Integer
    <1>
    Actual: java.lang.Integer
    <2>
    """
    And got the following notation:
    """
    <<a, c.value>><opt> 1
           ^
                    ^
    """
    Examples:
      | opt | text        |
      | :   | match       |
      | =   | be equal to |

  Scenario Outline: invoke property of group should also a group
    Given the following json:
    """
    {
      "a": {
        "value": 1
      },
      "b": {
        "value": 1
      },
      "c": {
        "value": 2
      }
    }
    """
    Then the following verification should pass:
    """
    <<a b>>.value<opt> 1
    """
    And the inspect should:
    """
    <<a, b>>.value<opt> 1
    """
    And evaluate by:
    """
    <<a, c>>.value<opt> 1
    """
    Then failed with the message:
    """
    Expected to <text>: java.lang.Integer
    <1>
    Actual: java.lang.Integer
    <2>
    """
    Examples:
      | opt | text        |
      | :   | match       |
      | =   | be equal to |

  Scenario: group node as left operand of expression
    Given the following json:
    """
    {
      "a": "hello",
      "b": "hello"
    }
    """
    Then the following verification should pass:
    """
    <<a b>> + 'World' + '!'= helloWorld!
    """
    And the inspect should:
    """
    <<a, b>> + 'World' + '!'= 'helloWorld!'
    """

  Scenario: group node as right operand of expression
    Given the following json:
    """
    {
      "a": "World",
      "b": "World",
      "c": {
        "value": "not eq"
      }
    }
    """
    Then the following verification should pass:
    """
    'hello' + <<a b>>= helloWorld
    """
    And the inspect should:
    """
    'hello' + <<a, b>>= 'helloWorld'
    """
    When evaluate by:
    """
    <<a, c.value>>= World
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.String
    <World>
    Actual: java.lang.String
    <not eq>
    """
    And got the following notation:
    """
    <<a, c.value>>= World
           ^
                    ^
    """

  Scenario: operator precedence in group node expression
    Given the following json:
    """
    {
      "a": 1,
      "b": 1,
      "c": {
        "value": 2
      }
    }
    """
    Then the following verification should pass:
    """
    1 + <<a b>>*2= 3
    """
    And the inspect should:
    """
    1 + <<a, b>> * 2= 3
    """

  Scenario: group node combine group node
    Given the following json:
    """
    {
      "a": 1,
      "b": 1,
      "c": 2,
      "d": 2
    }
    """
    Then the following verification should pass:
    """
    <<a b>> + <<c d>>= 3
    """
    And the inspect should:
    """
    <<a, b>> + <<c, d>>= 3
    """
    When evaluate by:
    """
    <<a b>> + <<c a>>= 3
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.Integer
    <3>
    Actual: java.lang.Integer
    <2>
    """
    And got the following notation:
    """
    <<a b>> + <<c a>>= 3
      ^
                  ^
                       ^
    """

  Rule: in object scope

    Scenario: use group node in object scope verification
      Given the following json:
      """
      {
        "value": {
          "a": 1,
          "b": 1,
          "c": 2
        }
      }
      """
      Then the following verification should pass:
      """
      value: {
        <<a b>>: 1
      }
      """
      And the inspect should:
      """
      value: {<<a, b>>: 1}
      """
      When evaluate by:
      """
      value: {
        <<a c>>: 1
      }
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <1>
      Actual: java.lang.Integer
      <2>
      """
      And got the following notation:
      """
      value: {
        <<a c>>: 1
            ^
                 ^
      }
      """

    Scenario: group node in = object scope verification
      Given the following json:
      """
      {
        "value": {
          "a": 1,
          "b": 1,
          "c": 1
        }
      }
      """
      Then the following verification should pass:
      """
      value= {
        <<a b c>>: 1
      }
      """
      And the inspect should:
      """
      value= {<<a, b, c>>: 1}
      """
      When evaluate by:
      """
      value= {
        <<a b>>: 1
      }
      """
      Then failed with the message:
      """
      Unexpected fields `c` in value
      """
      And got the following notation:
      """
      value= {
           ^
        <<a b>>: 1
      }
      """
