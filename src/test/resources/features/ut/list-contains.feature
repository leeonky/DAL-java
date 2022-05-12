Feature: list contains UT

  Scenario Outline: contains empty should always pass
    Given the following json:
    """
    <input>
    """
    Then the following verification should pass:
    """
    : [...  ...]
    """
    And the inspect should:
    """
    : [..., ...]
    """
    Examples:
      | input          |
      | []             |
      | ["any"]        |
      | ["any", "any"] |

  Scenario: contains one element verification
    Given the following json:
    """
    [ 1 ]
    """
    Then the following verification should pass:
    """
    : [... 1 ...]
    """
    And the inspect should:
    """
    : [..., : 1, ...]
    """

  Scenario: not pass when has only one unmatched element
    Given the following json:
    """
    [ 1 ]
    """
    When evaluate by:
    """
    : [..., 2 ...]
    """
    Then failed with the message:
    """
    No such element
    """
    And got the following notation:
    """
    : [..., 2 ...]
            ^
    """

  Scenario: passed when has matched element with different index
    Given the following json:
    """
    [ 1, 2 ]
    """
    Then the following verification should pass:
    """
    : [..., 2 ...]
    """

  Scenario: not pass when index is out of range
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
    : [..., 2 ...]
    """
    Then failed with the message:
    """
    No such element
    """
    And got the following notation:
    """
    : [..., 2 ...]
            ^
    """

  Scenario: contains two elements
    Given the following json:
    """
    [ 1, 2 ]
    """
    Then the following verification should pass:
    """
    : [... 1 2 ...]
    """
    And the inspect should:
    """
    : [..., : 1, : 2, ...]
    """
    When evaluate by:
    """
    : [... 1 3 ...]
    """
    Then failed with the message:
    """
    No such element
    """
    And got the following notation:
    """
    : [... 1 3 ...]
             ^
    """

  Scenario: contained element should order sensitive
    Given the following json:
    """
    [ 1, 2 ]
    """
    When evaluate by:
    """
    : [... 2 1 ...]
    """
    Then failed with the message:
    """
    No such element
    """
    And got the following notation:
    """
    : [... 2 1 ...]
             ^
    """
