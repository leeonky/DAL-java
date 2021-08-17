Feature: object judgement: equal to

  Scenario: empty object is equal to {}
    Given the following input data:
    """
      {}
    """
    Then the follow assertion should be passed:
    """
      = {}
    """

  Scenario: non empty object is not equal to {}
    Given the following input data:
    """
      {
        "key1": 100,
        "key2": 200
      }
    """
    When assert by the follow code:
    """
      = {}
    """
    Then failed with the following message:
    """
    unexpected fields `key1`, `key2`
    """
    And got the following source code information:
    """
      = {}
      ^
    """