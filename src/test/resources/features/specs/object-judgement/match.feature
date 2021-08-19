Feature: object: {...}

  Scenario Outline: any non-null object matches {}
    Given the following input data:
    """
      <data>
    """
    Then the follow assertion should be passed:
    """
      : {}
    """
    Examples:
      | data         |
      | {}           |
      | "any string" |
      | 100          |

  Scenario: null does not match {}
    When assert by the follow code:
    """
      null: {}
    """
    Then failed with the following message:
    """
    [null] does not match non-null object
    """
    And got the following source code information:
    """
      null: {}
      ^
    """

  Scenario: only match the expected field, ignore unexpected field
    Given the following input data:
    """
      {
        "key1": 1,
        "key2": 2
      }
    """
    Then the follow assertion should be passed:
    """
      : {
        key1: 1
      }
    """

  Scenario: any unmatched field value should not pass
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
      : {
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
      : {
        key1: 'v1'
        key2: 'unmatched'
              ^
        key3: 'v3'
      }
    """

#TODO move to basic judgement
  Scenario: non null does not match null
    When assert by the follow code:
    """
      1: null
    """
    Then failed with the following message:
    """
    [1] does not match null
    """
    And got the following source code information:
    """
      1: null
      ^
    """

#TODO move to basic judgement
  Scenario: null matches null
    Then the follow assertion should be passed:
    """
      null: null
    """
