Feature: verify list

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
    Different list size
    Expected: <1>
    Actual: <2>
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
    Expected to be equal to: java.lang.Integer
    <3>
     ^
    Actual: java.lang.Integer
    <2>
     ^
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
    Different list size
    Expected: <1>
    Actual: <2>
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
    Expected to match: java.lang.Integer
    <3>
     ^
    Actual: java.lang.Integer
    <2>
     ^
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

  Scenario: explicit element verification
    Given the following input data:
    """
      [1, 2]
    """
    Then the following assertion should pass:
    """
      = [1 :{...}]
    """
    And the following assertion should pass:
    """
      : [=1 {...}]
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
    Different list size
    Expected: <1>
    Actual: <2>
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
    Expected to be equal to: java.lang.Integer
    <4>
     ^
    Actual: java.lang.Integer
    <3>
     ^
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
    Invalid input value, expect a List but: java.lang.Integer
    <1>
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
    Invalid input value, expect a List but: java.lang.Integer
    <1>
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

  Scenario: support mapping list element property to new list by postfix `[]`
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
      list.data[].value = ['v1' 'v2']
    """
    And the following assertion should pass:
    """
      list: { data[].value: ['v1' 'v2'] }
    """

  Scenario: use mandatory [] after [xx] to mapping sub list element ot new list
    Given the following input data:
    """
      {"list": [[1,2], [1,2,3]]}
    """
    Then the following assertion should pass:
    """
      list[0][] = [1 1]
    """

  Scenario: use schema expression in object
    Given the following schema:
    """
    public class IdZero implements Schema {
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

  Scenario: use schema in verification expression
    Given the following schema:
    """
    @Partial
    public class IdZero implements Schema {
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
