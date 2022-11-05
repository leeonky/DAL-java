Feature: verify object

  Scenario: '= {}' means no unexpected fields and echo field value verification should pass
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
    Unexpected fields `key1`, `key2`
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
    Expected to match: java.lang.String
    <unmatched>
     ^
    Actual: java.lang.String
    <2>
     ^
    """
    And got the following source code information:
    """
      = {
        key1: '1'
        key2: 'unmatched'
              ^
      }
    """

  Scenario: ': {}' means ignore unexpected fields and echo field value verification should pass
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
    Expected to match: java.lang.String
    <unmatched>
     ^
    Actual: java.lang.String
    <2>
     ^
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
      = { }
    """

  Scenario Outline: any non-null object matches {}
    Given the following input data:
    """
      <data>
    """
    Then the following assertion should pass:
    """
      : {...}
    """
    Examples:
      | data         |
      | {}           |
      | "any string" |
      | 100          |

  Scenario: do not allow :{}
    Given the following input data:
    """
      {}
    """
    When evaluate by:
    """
      : {}
    """
    Then failed with the message:
    """
    Should use `{...}` to verify any non null object
    """
    And got the following notation:
    """
      : {}
        ^
    """

  Scenario: nested object verification
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
    Expected to match: java.lang.Integer
    <100>
     ^
    Actual: java.lang.Integer
    <3>
     ^
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

  Scenario: verification value can be calculation expression or regex
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
    Unexpected fields `key2`
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
    Expected to match: java.lang.Integer
    <100>
       ^
    Actual: java.lang.Integer
    <10>
       ^
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

  Scenario: use schema expression in object
    Given the following schema:
    """
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    Given the following input data:
    """
      {
        "key1": {
          "id": 0
        }
      }
    """
    Then the following assertion should pass:
    """
      = {
        key1 is IdZero
      }
    """

  Scenario: use schema in verification expression
    Given the following schema:
    """
    @Partial
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    Given the following input data:
    """
      {
        "key1": {
          "id": 0,
          "value": 100
        }
      }
    """
    Then the following assertion should pass:
    """
      = {
        key1 is IdZero: {
          value: 100
        }
      }
    """
