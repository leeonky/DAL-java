Feature: access list element by [n]

  Scenario Outline: access input list
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate by:
    """
      <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code    | value | inspect |
      | [0]     | 1     | [0]     |
      | [ 1 ]   | 2     | [1]     |
      | [\n2\n] | 3     | [2]     |

  Scenario Outline: access from the end of list
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate by:
    """
      <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code     | value | inspect |
      | [-1]     | 3     | [-1]    |
      | [ -2 ]   | 2     | [-2]    |
      | [\n-3\n] | 1     | [-3]    |

  Scenario: raise error when index out of bound
    Given the following json:
    """
      [1, 2]
    """
    When evaluate by:
    """
      [3]
    """
    Then failed with the message:
    """
    Index out of bounds (3), first index is: 0
    """
    And got the following notation:
    """
      [3]
      ^
    """
    When evaluate by:
    """
      [-3]
    """
    Then failed with the message:
    """
    Index out of bounds (-3), first index is: 0
    """
    And got the following notation:
    """
      [-3]
      ^
    """

  Scenario: raise error when list is null
    When evaluate by:
    """
    null['any']
    """
    Then failed with the message:
    """
    Instance is null
    """
    And got the following notation:
    """
    null['any']
        ^
    """
