Feature: list: [[...]]

  Scenario: should compare both list and sub list size and each element
    Given the following input data:
    """
      [[2, 3]]
    """
    Then the following assertion should be passed:
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

#  = [1, [2 3]] support comma
