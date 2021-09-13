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
    expected list size [1] but was [2]
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
    expected [2] equal to [3] but was not
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
    expected list size [1] but was [2]
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
    expected [2] matches [3] but was not
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

  Scenario: explicit judgement before element
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
      = [1+1 =1+2 :2+2 :/5/]
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
    expected list size [1] but was [2]
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
    expected [3] equal to [4] but was not
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

  Scenario: use ... at the end of list to ignore list size
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
