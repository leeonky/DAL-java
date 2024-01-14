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
    null: {...}
    """
    Then failed with the message:
    """
    The input value is null
    """
    And got the following notation:
    """
    null: {...}
          ^
    """
    And the inspect should:
    """
    null: {...}
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
    Expected to match: java.lang.String
    <Jack>
     ^
    Actual: java.lang.String
    <Tom>
     ^
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
    Method or property `string` does not exist in `#package#Data`
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

  Scenario: unexpected number key
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, String> data = new java.util.HashMap<Object, String>() {{
        put(0, "str1");
        put(2, "str2");
        put("a", "strA");
      }};
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    data= {
      0= str1
    }
    """
    Then failed with the message:
    """
    Unexpected fields `a`, 2 in data
    """
    When use a instance of java class "Data" to evaluate:
    """
    data= {
      {}= {
        0= str1
      }
    }
    """
    Then failed with the message:
    """
    Unexpected fields `a`, 2 in {}
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

  Scenario: verification chain
    Given the following json:
    """
    {
      "value": 5
    }
    """
    Then the following verification should pass:
    """
    = {
      value= 5
           = 2+3
    }
    """
    Then the following verification should pass:
    """
    = {
      value is Number
            is Number
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
    And the inspect should:
    """
    : {name: 'Tom', id: '001'}
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
    Expected to match: java.lang.String
    <002>
       ^
    Actual: java.lang.String
    <001>
       ^
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
    Should end with `}`
    """
    And got the following notation:
    """
    : {
       ^
    """

  Scenario: raise error when invalid verification expression
    When evaluate by:
    """
    : { (: 1 }
    """
    Then failed with the message:
    """
    Expect a object property
    """
    And got the following notation:
    """
    : { (: 1 }
        ^
    """

  Scenario: raise error when not verification expression
    When evaluate by:
    """
    : { a * 1 }
    """
    Then failed with the message:
    """
    Expect a verification operator
    """
    And got the following notation:
    """
    : { a * 1 }
          ^
    """

  Scenario: raise error when missing verification operator
    When evaluate by:
    """
    : { a }
    """
    Then failed with the message:
    """
    Expect a verification operator
    """
    And got the following notation:
    """
    : { a }
          ^
    """

  Scenario: expression in operand
    When the following json:
    """
    {"a": 2}
    """
    Then the following verification should pass:
    """
    : {
      a: 1+1
    }
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

  Scenario: tail comma is valid
    Given the following json:
    """
    {
      "value": "hello"
    }
    """
    Then the following verification should pass:
    """
    : {
      value: 'hello',
    }
    """
    But the following verification should syntax error:
    """
    : {,}
    """

  Scenario: support slash property
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
    : {
      user/name: Tom
    }
    """
    And the inspect should:
    """
    : {user/name: 'Tom'}
    """

  Scenario: support slash number property
    Given the following json:
    """
    {
      "list": [["hello"]]
    }
    """
    Then the following verification should pass:
    """
    list: {
      0 / 0: hello
    }
    """

  Scenario: start with meta property
    Given the following json:
    """
    {}
    """
    Then the following verification should pass:
    """
    : {
      ::object.class.simpleName= 'LinkedHashMap'
    }
    """
    And the inspect should:
    """
    : {::object.class.simpleName= 'LinkedHashMap'}
    """
