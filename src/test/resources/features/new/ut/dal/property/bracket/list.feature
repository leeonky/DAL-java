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
    Examples:
      | code    | value |
      | [0]     | 1     |
      | [ 1 ]   | 2     |
      | [\n2\n] | 3     |

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
    Examples:
      | code     | value |
      | [-1]     | 3     |
      | [-2]     | 2     |
      | [\n-3\n] | 1     |

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
    Index out of bounds (Index: 3, Size: 2)
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
    Index out of bounds (-1)
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
