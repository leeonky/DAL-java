Feature: access data

  Scenario: access data
    Given the following input data:
    """
      [1, 2, 3]
    """
    When evaluate list by the following code:
    """
      [0]
    """
    Then single result is:
    """
    : 1
    """

  Scenario: non null equal to null
    Given the following input data:
    """
      [1, 2, 3]
    """
    When evaluate list by the following code:
    """
      [0] ([1]) ([2])
    """
    Then multi result is:
    """
    : [1 2 3]
    """
