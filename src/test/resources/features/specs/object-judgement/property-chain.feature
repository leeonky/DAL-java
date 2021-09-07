Feature: {property.chain: ...}

  Scenario: support property chain judgement
    Given the following input data:
    """
      {
        "key1": {
          "s1": 3,
          "s2": 4
        },
        "key2": {
          "s1": 10,
          "s2": 20
        }
      }
    """
    Then the following assertion should pass:
    """
      : {
        key1.s1: 3
        key2.s1: 10
      }
    """

  Scenario: should not pass when got unexpected fields in equal to judgement
    Given the following input data:
    """
      {
        "key1": {
          "s1": 3
        },
        "key2": {
          "s1": 10
        }
      }
    """
    When assert by the following code:
    """
      = {
        key1.s1: 3
      }
    """
    Then failed with the following message:
    """
    unexpected fields `key2`
    """
    And got the following source code information:
    """
      = {
      ^
        key1.s1: 3
      }
    """

  Scenario: should not pass when any field judgement failed
    Given the following input data:
    """
      {
        "key1": {
          "s1": 3
        },
        "key2": {
          "s1": 10
        }
      }
    """
    When assert by the following code:
    """
      = {
        key1.s1: 3
        key2.s1: 100
      }
    """
    Then failed with the following message:
    """
    expected [10] matches [100] but was not
    """
    And got the following source code information:
    """
      = {
        key1.s1: 3
        key2.s1: 100
                 ^
      }
    """

#TODO .key: 1
