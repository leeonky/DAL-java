Feature: judge object

  Scenario: '= {}' means no unexpected fields and echo field value judgement should pass
    Given the following input data:
    """
      {
        "key1": "1",
        "key2": "2"
      }
    """
    Then the following assertion should pass:
    """
      = {
        key1: '1'
        key2= '2'
      }
    """
    When assert by the following code:
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
    When assert by the following code:
    """
      = {
        key1: '1'
        key2: 'unmatched'
      }
    """
    Then failed with the following message:
    """
    expected ['2'] matches ['unmatched'] but was not
    """
    And got the following source code information:
    """
      = {
        key1: '1'
        key2: 'unmatched'
              ^
      }
    """

  Scenario: ': {}' means ignore unexpected fields and echo field value judgement should pass
    Given the following input data:
    """
      {
        "key1": "1",
        "key2": "2"
      }
    """
    Then the following assertion should pass:
    """
      : {
        key1= "1"
      }
    """
    When assert by the following code:
    """
      : {
        key1: '1'
        key2: 'unmatched'
      }
    """
    Then failed with the following message:
    """
    expected ['2'] matches ['unmatched'] but was not
    """
    And got the following source code information:
    """
      : {
        key1: '1'
        key2: 'unmatched'
              ^
      }
    """

  Scenario: empty object is equal to {}
    Given the following input data:
    """
      {}
    """
    Then the following assertion should pass:
    """
      = {}
    """

  Scenario Outline: any non-null object matches {}
    Given the following input data:
    """
      <data>
    """
    Then the following assertion should pass:
    """
      : {}
    """
    Examples:
      | data         |
      | {}           |
      | "any string" |
      | 100          |

  Scenario: nested object judgement
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
    Then the following assertion should pass:
    """
      : {
        key1: 1
        key2= {
          s1: 3
          s2: 4
        }
      }
    """
    When assert by the following code:
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

  Scenario: judgement value can be calculation expression or regex
    Given the following input data:
    """
      {
        "key1": 2,
        "key2": 3
      }
    """
    Then the following assertion should pass:
    """
      = {
        key1= 1+1
        key2: /3/
      }
    """

  Scenario: field can be a property chain
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

  Scenario: use comma to avoid ambiguous field
    Given the following input data:
    """
      {
        "key1": "1",
        "key2": "2"
      }
    """
    Then the following assertion should pass:
    """
      = {
        .key1: '1',
        .key2= '2'
      }
    """

#  TODO nested list sub alias
#  TODO property chain sub alias
#  TODO process size property of list