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

  Scenario: one verification expression
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

  Scenario: nested object verification
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
      user: {
        name: 'Tom'
      }
    }
    """

  Scenario: two verification expressions
    Given the following json:
    """
    {
      "name": "Tom",
      "id": "001"
    }
    """
    And the following verification should pass:
    """
    : {
      name: 'Tom'
      id: '001'
    }
    """
    When evaluate by:
    """
    = {
      name: 'Tom'
      id: '002'
    }
    """
    Then failed with the message:
    """
    Expecting java.lang.String
    <001>
    to match java.lang.String
    <002>
    but was not
    """
    And got the following notation:
    """
    = {
      name: 'Tom'
      id: '002'
          ^
    }
    """

  Scenario: raise error when no closing brace
    When evaluate by:
    """
    : {
    """
    Then failed with the message:
    """
    should end with `}`
    """
    And got the following notation:
    """
    : {
       ^
    """

  Scenario: raise error when operand is invalid
    When evaluate by:
    """
    : { name: + }
    """
    Then failed with the message:
    """
    expect a value or expression
    """
    And got the following notation:
    """
    : { name: + }
              ^
    """

  Scenario: raise error when invalid verification expression
    When evaluate by:
    """
    : { 1: 1 }
    """
    Then failed with the message:
    """
    expect a object property
    """
    And got the following notation:
    """
    : { 1: 1 }
        ^
    """

  Scenario: raise error when not verification expression
    When evaluate by:
    """
    : { a + 1 }
    """
    Then failed with the message:
    """
    expect operator `:` or `=`
    """
    And got the following notation:
    """
    : { a + 1 }
          ^
    """

  Scenario: raise error when missing verification operator
    When evaluate by:
    """
    : { a 1 }
    """
    Then failed with the message:
    """
    expect operator `:` or `=`
    """
    And got the following notation:
    """
    : { a 1 }
          ^
    """

  Scenario: support optional comma between after sub expression
    Given the following json:
    """
    {
      "name": "Tom",
      "id": "001"
    }
    """
    And the following verification should pass:
    """
    : {
      name: 'Tom'
      id: '001'
    }
    """

  Scenario: support schema expression
    Given the following json:
    """
    {
      "value": 100
    }
    """
    Then the following verification should pass:
    """
     : {
       value is Integer
     }
    """
    And the following verification should failed:
    """
    : {
      value is String
    }
    """
    And got the following notation:
    """
    : {
      value is String
               ^
    }
    """

  Scenario: support schema in verification expression
    Given the following json:
    """
    {
      "value": "hello"
    }
    """
    Then the following verification should pass:
    """
    : {
      value is String: 'hello'
    }
    """
    And the following verification should failed:
    """
    : {
      value is String: 'world'
    }
    """
    And got the following notation:
    """
    : {
      value is String: 'world'
                       ^
    }
    """

#    TODO support valid {a: 1,} invalid {,}


