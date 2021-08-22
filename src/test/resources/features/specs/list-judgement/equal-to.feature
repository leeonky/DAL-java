Feature: list= [...]

  Scenario: should compare both list size and each element
    Given the following input data:
    """
      [1, 2]
    """
    Then the following assertion should be passed:
    """
      = [1 2]
    """

  Scenario: empty list equal to empty list
    Given the following input data:
    """
      []
    """
    Then the following assertion should be passed:
    """
      = []
    """

  Scenario: should not pass when unequal list size
    Given the following input data:
    """
      [1, 2]
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

  Scenario: should not pass when any unequal list element
    Given the following input data:
    """
      [1, 2]
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

#  TODO null equal to []
#  TODO match
#  TODO equal
#  TODO skip element
#  TODO ignore tails
#  TODO element match
#  TODO element equal
#  TODO nested object
#  TODO sub alias
#  TODO support comma
