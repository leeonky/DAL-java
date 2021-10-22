Feature: judge list

  Scenario: '= []' means both list size and element at the same index should be equal
    Given the following input data:
    """
      [1, 2]
    """
    Then the following assertion should pass:
    """
      = [1 2]
    """
    When assert by the following code:
    """
      = [1]
    """
    Then failed with the following message:
    """
    Expecting list size to be <1> but was <2>
    """
    And got the following source code information:
    """
      = [1]
        ^
    """
    When assert by the following code:
    """
      = [1 3]
    """
    Then failed with the following message:
    """
    Expecting java.lang.Integer
    <2>
    to be equal to java.lang.Integer
    <3>
    but was not
    """
    And got the following source code information:
    """
      = [1 3]
           ^
    """

  Scenario: ': []' means the same list size and element at the same index should match
    Given the following input data:
    """
      [1, 2]
    """
    And the following assertion should pass:
    """
      : [1 2]
    """
    When assert by the following code:
    """
      : [1]
    """
    Then failed with the following message:
    """
    Expecting list size to be <1> but was <2>
    """
    And got the following source code information:
    """
      : [1]
        ^
    """
    When assert by the following code:
    """
      : [1 3]
    """
    Then failed with the following message:
    """
    Expecting java.lang.Integer
    <2>
    to match java.lang.Integer
    <3>
    but was not
    """
    And got the following source code information:
    """
      : [1 3]
           ^
    """

  Scenario: empty list
    Given the following input data:
    """
      []
    """
    Then the following assertion should pass:
    """
      : []
    """
    And the following assertion should pass:
    """
      = []
    """

  Scenario: explicit element judgement
    Given the following input data:
    """
      [1, 2]
    """
    Then the following assertion should pass:
    """
      = [1 :{}]
    """
    And the following assertion should pass:
    """
      : [=1 {}]
    """

  Scenario: element can be calculation expression or regex
    Given the following input data:
    """
      [2, 3, 4, 5]
    """
    Then the following assertion should pass:
    """
      : [1+1 =1+2 :2+2, /5/]
    """

  Scenario: multidimensional list
    Given the following input data:
    """
      [[2, 3]]
    """
    Then the following assertion should pass:
    """
      = [[2 3]]
    """

  Scenario: should not pass when any list or sub list size or element assertion failure
    Given the following input data:
    """
      [[2, 3]]
    """
    When assert by the following code:
    """
      = [[2]]
    """
    Then failed with the following message:
    """
    Expecting list size to be <1> but was <2>
    """
    And got the following source code information:
    """
      = [[2]]
         ^
    """
    When assert by the following code:
    """
      = [[2 4]]
    """
    Then failed with the following message:
    """
    Expecting java.lang.Integer
    <3>
    to be equal to java.lang.Integer
    <4>
    but was not
    """
    And got the following source code information:
    """
      = [[2 4]]
            ^
    """

  Scenario: use comma to avoid ambiguous element
    Given the following input data:
    """
      [1, [2, 3]]
    """
    Then the following assertion should pass:
    """
      = [1, [2 3]]
    """

  Scenario: use * to force positive judgment
    Given the following input data:
    """
      [1, null]
    """
    Then the following assertion should pass:
    """
      = [* *]
    """
    And the following assertion should pass:
    """
      : [* *]
    """

  Scenario: use ... at the end of list to ignore list size and [...] to check list type
    Given the following input data:
    """
      [1, 2, 3]
    """
    Then the following assertion should pass:
    """
      = [1 2 ...]
    """
    And the following assertion should pass:
    """
      : [1 2 ...]
    """
    And the following assertion should pass:
    """
      = [...]
    """
    And the following assertion should pass:
    """
      : [...]
    """
    When assert by the following code:
    """
    1 = [...]
    """
    Then failed with the following message:
    """
    Cannot compare java.lang.Integer
    <1>
    and list
    """
    And got the following source code information:
    """
    1 = [...]
        ^
    """
    When assert by the following code:
    """
    1 : [...]
    """
    Then failed with the following message:
    """
    Cannot compare java.lang.Integer
    <1>
    and list
    """
    And got the following source code information:
    """
    1 : [...]
        ^
    """

  Scenario: use ... at first of list to ignore list size and assert element from last
    Given the following input data:
    """
      [1, 2, 3]
    """
    Then the following assertion should pass:
    """
      = [... 3]
    """
    And the following assertion should pass:
    """
      : [... 3]
    """

  Scenario: support mapping list element property to new list by optional `.@`
    Given the following input data:
    """
      {"list": [{
        "data": {
          "value": "v1"
        }
      }, {
        "data": {
          "value": "v2"
        }
      }]}
    """
    Then the following assertion should pass:
    """
      list.@.data.value = ['v1' 'v2'] and
      list.data.value = ['v1' 'v2']
    """
    And the following assertion should pass:
    """
      list: { data.value: ['v1' 'v2'] }
    """

  Scenario: should check each element fields when use list = {} mapping
    Given the following input data:
    """
      {"list": [{
        "data": {
          "value": "v1"
        }
      }, {
        "data": {
          "value": "v2"
        },
        "unexpected": "any str"
      }]}
    """
    And assert by the following code:
    """
      list= { data.value: ['v1' 'v2'] }
    """
    Then failed with the following message:
    """
    Unexpected fields `unexpected` in list[1]
    """

  Scenario: use @size to mapping sub list size ot new list
    Given the following input data:
    """
      {"list": [[1,2], [1,2,3]]}
    """
    Then the following assertion should pass:
    """
      list.@size = [2 3]
    """

  Scenario: use mandatory .@ before [xx] to mapping sub list element ot new list
    Given the following input data:
    """
      {"list": [[1,2], [1,2,3]]}
    """
    Then the following assertion should pass:
    """
      list.@[0] = [1 1]
    """

  Scenario: use schema expression in object
    Given the following schema:
    """
    public class IdZero {
        public int id = 0;
    }
    """
    Given the following input data:
    """
      [{ "id": 0 }]
    """
    Then the following assertion should pass:
    """
      = [is IdZero]
    """

  Scenario: use schema in judgement expression
    Given the following schema:
    """
    @Partial
    public class IdZero {
        public int id = 0;
    }
    """
    Given the following input data:
    """
      [
        {
          "id": 0,
          "value": 100
        }
      ]
    """
    Then the following assertion should pass:
    """
      = [
        is IdZero: {
          value: 100
        }
      ]
    """
