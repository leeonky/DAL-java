Feature: group

  Scenario: support group with << >> and verification with same value
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
    <<a b>>: 1
    """
    And the inspect should:
    """
    <<a, b>>: 1
    """
    And evaluate by:
    """
    <<a, c.value>>: 1
    """
    Then failed with the message:
    """
    Expected to match: java.lang.Integer
    <1>
     ^
    Actual: java.lang.Integer
    <2>
     ^
    """
    And got the following notation:
    """
    <<a, c.value>>: 1
           ^        ^
    """
    Then the following verification should pass:
    """
    <<a b>>= 1
    """
    And the inspect should:
    """
    <<a, b>>= 1
    """
    And evaluate by:
    """
    <<a, c.value>>= 1
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.Integer
    <1>
     ^
    Actual: java.lang.Integer
    <2>
     ^
    """
    And got the following notation:
    """
    <<a, c.value>>= 1
           ^        ^
    """

  Scenario: use `[]` in group node
    Given the following json:
    """
    {
      "a": 1,
      "b": 1
    }
    """
    Then the following verification should pass:
    """
    : {
      <<[a], [b]>>: 1,
      <<['a'], ['b']>>: 1
    }
    """

  Scenario: invoke property of group should also a group
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
    <<a b>>.value: 1
    """
    And the inspect should:
    """
    <<a, b>>.value: 1
    """
    And evaluate by:
    """
    <<a, c>>.value: 1
    """
    Then failed with the message:
    """
    Expected to match: java.lang.Integer
    <1>
     ^
    Actual: java.lang.Integer
    <2>
     ^
    """
    Then the following verification should pass:
    """
    <<a b>>.value= 1
    """
    And the inspect should:
    """
    <<a, b>>.value= 1
    """
    And evaluate by:
    """
    <<a, c>>.value= 1
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.Integer
    <1>
     ^
    Actual: java.lang.Integer
    <2>
     ^
    """

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
     ^
    Actual: java.lang.String
    <not eq>
     ^
    """
    And got the following notation:
    """
    <<a, c.value>>= World
           ^        ^
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
     ^
    Actual: java.lang.Integer
    <2>
     ^
    """
    And got the following notation:
    """
    <<a b>> + <<c a>>= 3
      ^           ^    ^
    """

  Scenario: group node in property chain and convert to currying method arg type
    Given the following json:
    """
    {
      "list": [{
        "value": 100
      }, {
        "value": 100
      }]
    }
    """
    Then the following verification should pass:
    """
    list<<0 1>>.value= 100
    """
    And the inspect should:
    """
    list<<0, 1>>.value= 100
    """
    Then the following verification should pass:
    """
    list<<'0' '1'>>.value= 100
    """
    And the inspect should:
    """
    list<<'0', '1'>>.value= 100
    """

  Scenario: use group property without dot
    Given the following json:
    """
    {
      "list": {
        "a": {
          "a": {
            "value": 100
          }
        },
        "b": {
          "value": 100
        }
      }
    }
    """
    Then the following verification should pass:
    """
    list<<a.a b>>.value= 100
#    list.<<a.a b>>.value= 100
    """

  Scenario: group node should return last experssion result
    Given the following json:
    """
    {
      "a": 1,
      "b": 2,
      "c": 3,
      "d": 4
    }
    """
    When evaluate by:
    """
    <<a b>>
    """
    Then the result should:
    """
    : 2
    """
    When evaluate by:
    """
    <<a b>> + <<c d>>
    """
    Then the result should:
    """
    : 6
    """

  Scenario: which / is after group
    Given the following json:
    """
    {
      "a": 1,
      "b": 2,
      "c": "str"
    }
    """
    When evaluate by:
    """
    <<a b>> which toString
    """
    Then the result should:
    """
    : '2'
    """
    Then the following verification should pass:
    """
    <<a b>> is Number
    """
    When evaluate by:
    """
    <<a c>> is Number
    """
    Then failed with the message:
    """
    Expected c to match schema `Number` but was not
    """
    And got the following notation:
    """
    <<a c>> is Number
        ^      ^
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
        <<c a>>: 1
      }
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <1>
       ^
      Actual: java.lang.Integer
      <2>
       ^
      """
      And got the following notation:
      """
      value: {
        <<c a>>: 1
          ^      ^
      }
      """

    Scenario: use group node in object scope verification for collection input
      Given the following json:
      """
      {
        "list": [1, 1, 2]
      }
      """
      Then the following verification should pass:
      """
      list: {
        <<0 1>>: 1,
        <<[0], [1]>>: 1
      }
      """
      And the inspect should:
      """
      list: {<<0, 1>>: 1, <<[0], [1]>>: 1}
      """
      When evaluate by:
      """
      list: {
        <<1 2>>: 1
      }
      """
      Then failed with the message:
      """
      Expected to match: java.lang.Integer
      <1>
       ^
      Actual: java.lang.Integer
      <2>
       ^
      """
      And got the following notation:
      """
      list: {
        <<1 2>>: 1
            ^    ^
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

    Scenario: could ignore << >> in key
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
        a, b, c: 1
      }
      """
      And the inspect should:
      """
      value= {<<a, b, c>>: 1}
      """
