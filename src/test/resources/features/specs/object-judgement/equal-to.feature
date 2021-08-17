Feature: object= {...}

  Scenario: should compare both object fields and values
    Given the following input data:
    """
      {
        "key1": 1,
        "key2": 2
      }
    """
    Then the follow assertion should be passed:
    """
      = {
        key1: 1
        key2: 2
      }
    """

  Scenario: empty object is equal to {}
    Given the following input data:
    """
      {}
    """
    Then the follow assertion should be passed:
    """
      = {}
    """

  Scenario: unexpected fields should not pass
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

  Scenario: any unequal field value should not pass
    Given the following input data:
    """
      {
        "key1": "v1",
        "key2": "v2",
        "key3": "v3"
      }
    """
    When assert by the follow code:
    """
      = {
        key1: 'v1'
        key2: 'unmatched'
        key3: 'v3'
      }
    """
    Then failed with the following message:
    """
    expected ['v2'] matches ['unmatched'] but was not
    """
    And got the following source code information:
    """
      = {
        key1: 'v1'
        key2: 'unmatched'
        ^
        key3: 'v3'
      }
    """




#  TODO property
#  TODO nested object
#  TODO property chain
#  TODO process getClass property for java bean and size property of list
#  TODO property is alias
#  TODO sub alias
