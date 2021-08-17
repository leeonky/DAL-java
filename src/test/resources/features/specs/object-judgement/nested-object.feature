Feature: {nested: {...}}

  Scenario: support judge nested object
    Given the following input data:
    """
      {
        "key1": 1,
        "key2": {
          "s1": 3,
          "s2": 4
        }
      }
    """
    Then the follow assertion should be passed:
    """
      : {
        key1: 1
        key2= {
          s1: 3
          s2: 4
        }
      }
    """

  Scenario: any failed of sub object judgement should not pass
    Given the following input data:
    """
      {
        "key1": 1,
        "key2": {
          "s1": 3,
          "s2": 4
        }
      }
    """
    When assert by the follow code:
    """
      : {
        key2: {
          s1: 100
        }
      }
    """
    Then failed with the following message:
    """
    expected [3] matches [100] but was not
    """
    And got the following source code information:
    """
      : {
        key2: {
          s1: 100
          ^
        }
      }
    """
