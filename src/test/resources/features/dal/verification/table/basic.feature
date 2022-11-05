Feature: basic verification via table

  Scenario: table one header and no rows
    Given the following json:
    """
    []
    """
    Then the following verification should pass:
    """
    : | name |
    """
    And the inspect should:
    """
    : | name |
    """
    And the following verification should pass:
    """
    = | name |
    """
    Given the following json:
    """
    [1]
    """
    When evaluate by:
    """
     : | name |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <0>
    Actual: <1>
    """
    And got the following notation:
    """
     : | name |
       ^
    """

  Scenario: table two header and no rows
    Given the following json:
    """
    []
    """
    Then the following verification should pass:
    """
    : | name | age |
    """
    And the inspect should:
    """
    : | name | age |
    """
    And the following verification should pass:
    """
    = | name | age |
    """
    Given the following json:
    """
    [1]
    """
    When evaluate by:
    """
     : | name | age |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <0>
    Actual: <1>
    """
    And got the following notation:
    """
     : | name | age |
       ^
    """

  Scenario: table one header and one row
    Given the following json:
    """
    [{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    : | name  |
      | 'Tom' |
    """
    And the inspect should:
    """
    : | name |
    | : 'Tom' |
    """
    Given the following json:
    """
    [{
      "name": "John"
    }]
    """
    When evaluate by:
    """
    : | name  |
      | 'Tom' |
    """
    Then failed with the message:
    """
    Expected to match: java.lang.String
    <Tom>
     ^
    Actual: java.lang.String
    <John>
     ^
    """
    And got the following notation:
    """
    : | name  |
      | 'Tom' |
        ^
    ^^^^^^^^^^^^
    """

  Scenario: table two header and two row
    Given the following json:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "Lucy",
      "age": 15
    }]
    """
    Then the following verification should pass:
    """
    : | name   | age |
      | 'Tom'  | 10  |
      | 'Lucy' | 15  |
    """
    And the inspect should:
    """
    : | name | age |
    | : 'Tom' | : 10 |
    | : 'Lucy' | : 15 |
    """
    Given the following json:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "Lucy",
      "age": 20
    }]
    """
    When evaluate by:
    """
    : | name   | age |
      | 'Tom'  | 10  |
      | 'Lucy' | 15  |
    """
    Then failed with the message:
    """
    Expected to match: java.lang.Integer
    <15>
     ^
    Actual: java.lang.Integer
    <20>
     ^
    """
    And got the following notation:
    """
    : | name   | age |
      | 'Tom'  | 10  |
      | 'Lucy' | 15  |
                 ^
    ^^^^^^^^^^^^^^^^^^^
    """

  Scenario: verification table by table verification
    Given the following json:
    """
    [{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    = | name   |
      | 'Tom'  |
    """
    And the inspect should:
    """
    = | name |
    | = 'Tom' |
    """
    Given the following json:
    """
    [{
      "name": "John",
      "age": 10
    }]
    """
    When evaluate by:
    """
     = | name   |
       | 'John' |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in [0]
    """
    And got the following notation:
    """
     = | name   |
     ^
       | 'John' |
    ^^^^^^^^^^^^^^
    """

  Scenario: compile table with row verification operator which has higher priority than table verification operator
    Given the following json:
    """
    [{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    : | name   |
    = | 'Tom'  |
    """
    And the inspect should:
    """
    : | name |
    = | = 'Tom' |
    """
    Given the following json:
    """
    [{
      "name": "John",
      "age": 10
    }]
    """
    When evaluate by:
    """
    : | name   |
    = | 'John' |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in [0]
    """
    And got the following notation:
    """
    : | name   |
    = | 'John' |
    ^
    ^^^^^^^^^^^^^
    """

  Scenario: compile table and specified header verification operator which has higher priority than row verification operator
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | user=         |
    : | {name: 'Tom'} |
    """
    And the inspect should:
    """
    : | user= |
    : | = {name: 'Tom'} |
    """
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom",
        "age": 10
      }
    }]
    """
    When evaluate by:
    """
    : | user=         |
    : | {name: 'Tom'} |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in user
    """
    And got the following notation:
    """
    : | user=         |
            ^
    : | {name: 'Tom'} |
    ^^^^^^^^^^^^^^^^^^^^
    """

  Scenario: compile table and specified cell verification operator which has higher priority than header verification operator
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | user:           |
    : | = {name: 'Tom'} |
    """
    And the inspect should:
    """
    : | user: |
    : | = {name: 'Tom'} |
    """
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom",
        "age": 10
      }
    }]
    """
    When evaluate by:
    """
    : | user:           |
    : | = {name: 'Tom'} |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in user
    """
    And got the following notation:
    """
    : | user:           |
    : | = {name: 'Tom'} |
        ^
    ^^^^^^^^^^^^^^^^^^^^^^
    """

  Scenario: is clause after table
    Given the following json:
    """
    [[{"name": "Tom", "age": 10}], "string"]
    """
    Then the following verification should pass:
    """
    : [
      | name  | age |
      | 'Tom' | 10  |
      is String
    ]
    """
    And the inspect should:
    """
    : [[0]: | name | age |
    | : 'Tom' | : 10 |, [1] is String]
    """

  Scenario: two table
    Given the following json:
    """
    [[{"name": "Tom", "age": 10}], [{"name": "John", "age": 15}]]
    """
    Then the following verification should pass:
    """
    :[
      | name  | age |
      | 'Tom' | 10  |
      ,
      | name   | age |
      | 'John' | 15  |
    ]
    """
    And the inspect should:
    """
    : [[0]: | name | age |
    | : 'Tom' | : 10 |, [1]: | name | age |
    | : 'John' | : 15 |]
    """
    Then the following verification should pass:
    """
    :[
      | name  | age |
      | 'Tom' | 10  |,
      | name   | age |
      | 'John' | 15  |
    ]
    """
    And the inspect should:
    """
    : [[0]: | name | age |
    | : 'Tom' | : 10 |, [1]: | name | age |
    | : 'John' | : 15 |]
    """
    Then the following verification should pass:
    """
    :[
      | name  | age |
      | 'Tom' | 10  |

      | name   | age |
      | 'John' | 15  |
    ]
    """
    And the inspect should:
    """
    : [[0]: | name | age |
    | : 'Tom' | : 10 |, [1]: | name | age |
    | : 'John' | : 15 |]
    """

  Scenario Outline: property chain in header
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | user<opt>name |
      | Tom       |
    """
    And the inspect should:
    """
    : | user<opt>name |
    | : 'Tom' |
    """
    Examples:
      | opt |
      | .   |
      | /   |

  Scenario: sort list with table
    Given the following json:
    """
    [1, 3, 2]
    """
    Then the following verification should pass:
    """
    : | +{} |
      | 1   |
      | 2   |
      | 3   |
    """

  Scenario: number type key
    Given the following java class:
    """
    public class Data {
      public java.util.List<Object> data = java.util.Arrays.asList(new java.util.HashMap<Object, String>() {{
        put(0, "str1");
        put(2, "str2");
        put("a", "strA");
      }});
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data: | 0    | 2    | a    |
          | str1 | str2 | strA |
    """
    And the inspect should:
    """
    data: | 0 | 2 | a |
    | : 'str1' | : 'str2' | : 'strA' |
    """

  Scenario: number is row key
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, Object> data = new java.util.HashMap<Object, Object>() {{
        put(0, new java.util.HashMap<Object, String>() {{
          put("a", "0a");
          put("b", "0b");
        }});
        put(1, new java.util.HashMap<Object, String>() {{
          put("a", "1a");
          put("b", "1b");
        }});
      }};
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data=  | a    | b    |
         0 | '0a' | '0b' |
         1 | '1a' | '1b' |
    """
    And the inspect should:
    """
    data= | a | b |
    0 | = '0a' | = '0b' |
    1 | = '1a' | = '1b' |
    """
    When use a instance of java class "Data" to evaluate:
    """
    data= | a    | b    |
        1 | '1a' | '1b' |
    """
    Then failed with the message:
    """
    Unexpected fields 0 in data
    """
    And got the following notation:
    """
    data= | a    | b    |
        ^
        1 | '1a' | '1b' |
    """

  Scenario: mixed number and string property
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, Object> data = new java.util.HashMap<Object, Object>() {{
        put(0, new java.util.HashMap<Object, String>() {{
          put("a", "0a");
          put("b", "0b");
        }});
        put("1", new java.util.HashMap<Object, String>() {{
          put("a", "1a");
          put("b", "1b");
        }});
      }};
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data: | a    | b    |
        0 | '0a' | '0b' |
      '1' | '1a' | '1b' |
    """
    And the inspect should:
    """
    data: | a | b |
    0 | : '0a' | : '0b' |
    '1' | : '1a' | : '1b' |
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data: | a    | b    |
      '1' | '1a' | '1b' |
        0 | '0a' | '0b' |
    """
    And the inspect should:
    """
    data: | a | b |
    '1' | : '1a' | : '1b' |
    0 | : '0a' | : '0b' |
    """

  Scenario: header is meta property
    Given the following json:
    """
    [{}]
    """
    Then the following verification should pass:
    """
    : | ::object.class.simpleName  |
      | LinkedHashMap              |
    """
    And the inspect should:
    """
    : | ::object.class.simpleName |
    | : 'LinkedHashMap' |
    """
