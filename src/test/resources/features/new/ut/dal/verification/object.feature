Feature: object

  Scenario: verify empty object
    When the following json:
    """
    {}
    """
    Then the following verification should pass:
    """
    = {}
    """
    When evaluate by:
    """
    null: {}
    """
    Then failed with the message:
    """
    The input value is null
    """
    And got the following notation:
    """
    null: {}
          ^
    """
    And the inspect should:
    """
    null: {}
    """

  Scenario: unexpected fields
    When the following json:
    """
    {"a": 1}
    """
    And evaluate by:
    """
      = {}
    """
    Then failed with the message:
    """
    Unexpected fields `a`
    """
    And got the following notation:
    """
      = {}
      ^
    """

  Scenario: support one judgement expression
    Given the following json:
    """
    {
      "name": "Tom"
    }
    """
    Then the following verification should pass:
    """
    : {
      name= 'Tom'
    }
    """
    And the following verification should pass:
    """
    = {
      name= 'Tom'
    }
    """
    And the following verification should pass:
    """
    : {
      name: 'Tom'
    }
    """
    And the following verification should pass:
    """
    = {
      name: 'Tom'
    }
    """
    When evaluate by:
    """
    = {
      name: 'Jack'
    }
    """
    Then failed with the message:
    """
    Expecting java.lang.String
    <Tom>
    to match java.lang.String
    <Jack>
    but was not
    """
    And got the following notation:
    """
    = {
      name: 'Jack'
            ^
    }
    """

  Scenario: not exist key
    Given the following java class:
    """
    public class Data {
      public int value = 100;
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    : {
      string: 100
    }
    """
    Then failed with the message:
    """
    Get property `string` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `string` does not exist in `Data`
    """
    And got the following notation:
    """
    : {
      string: 100
      ^
    }
    """

  Scenario: unexpected fields-2
    Given the following json:
    """
    {
      "name": "Tom",
      "id": "001",
      "age": 10
    }
    """
    When evaluate by:
    """
    = {
      name: 'Tom'
    }
    """
    Then failed with the message:
    """
    Unexpected fields `id`, `age`
    """
    And got the following notation:
    """
    = {
    ^
      name: 'Tom'
    }
    """

  Scenario: property chain
    Given the following json:
    """
    {
      "user": {
        "name": "Tom"
      }
    }
    """
    Then the following verification should pass:
    """
    = {
      user.name: 'Tom'
    }
    """


#    TODO support valid {a: 1,} invalid {,}
