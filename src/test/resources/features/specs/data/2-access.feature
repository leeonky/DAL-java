Feature: access data

  Scenario: return input value when no code
    Given the following json:
    """
    1
    """
    When evaluate by:
    """
    """
    Then the result should:
    """
    = 1
    """

  Scenario: access list element by index
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate by:
    """
      [0]
    """
    Then the result should:
    """
    : 1
    """

  Scenario: evaluate all as a list
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate all by:
    """
      [0] ([1]) ([2])
    """
    Then the result should:
    """
    : [1 2 3]
    """
