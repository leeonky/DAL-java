Feature: compile table node

  Scenario: compile table header with only header and use table default judgement operator
    Given the following dal code:
    """
    | name |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| name: |'
      headers: [{
        property.inspect: 'name'
        operator.class.simpleName: 'Matcher'
      }]
    }
    """

  Scenario: compile table header with only header and header judgement operator was specified
    Given the following dal code:
    """
    | name= |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| name= |'
      headers: [{
        property.inspect: 'name'
        operator.class.simpleName: 'Equal'
      }]
    }
    """

  Scenario: compile table with header and cells and cell use default row judgement operator
    Given the following dal code:
    """
    | name= |
    | 'Tom' |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: "| name= |
    | = 'Tom' |"
      rows: [[{
        leftOperand.inspect: 'name'
        operator.class.simpleName: 'Equal'
        rightOperand.inspect: "'Tom'"
        }]]
    }
    """

  Scenario: compile table header with sort constructors
    Given the following dal code:
    """
    | ++name | -age |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| ++ name: | - age: |'
      headers: | property.inspect | sequence.value | sequence.type |
               | 'name'           | 2              | 'AZ'          |
               | 'age'            | 1              | 'ZA'          |
    }
    """

  Scenario: compile table header with another style of sort constructors
    Given the following dal code:
    """
    | ↑↑ name | ↓ age |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| ↑↑ name: | ↓ age: |'
      headers: | property.inspect | sequence.value | sequence.type |
               | 'name'           | 2              | 'AZ'          |
               | 'age'            | 1              | 'ZA'          |
    }
    """

  Scenario: compile table and cell use cell judgement operator
    Given the following dal code:
    """
    | name=  |
    | :'Tom' |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: "| name= |
    | : 'Tom' |"
      rows: [[{
        leftOperand.inspect: 'name'
        operator.class.simpleName: 'Matcher'
        rightOperand.inspect: "'Tom'"
        }]]
    }
    """

  Scenario: assert list data
    Given the following input data:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "Lucy",
      "age": 14
    }]
    """
    Then the following assertion should pass:
    """
    = | name   | age |
      | 'Tom'  | 10  |
      | 'Lucy' | 14  |
    """
    And the following assertion should pass:
    """
    : | name   | age  |
      | 'Tom'  | 10.0 |
      | 'Lucy' | 14.0 |
    """
    When assert by the following code:
    """
    = | name   | age  |
      | 'Tom'  | 10.0 |
      | 'Lucy' | 14   |
    """
    Then failed with the following message:
    """
    Expecting java.lang.Integer
    <10>
    to be equal to java.lang.Double
    <10.0>
    but was not
    """
    And got the following source code information:
    """
    = | name   | age  |
      | 'Tom'  | 10.0 |
                 ^
    ^^^^^^^^^^^^^^^^^^^
      | 'Lucy' | 14   |
    """
    When assert by the following code:
    """
    = | name   |
      | 'Tom'  |
      | 'Lucy' |
    """
    Then failed with the following message:
    """
    Unexpected fields `age` in [0]
    """
    And got the following source code information:
    """
    = | name   |
    ^
      | 'Tom'  |
    ^^^^^^^^^^^^
      | 'Lucy' |
    """

  Scenario: syntax error too many headers
    Given the following dal code:
    """
    | name  |
    | 'Tom' | 30 |
    """
    Then failed to get "table" node with the following message:
    """
    Different cell size
    """
    And got the following source code information:
    """
    | name  |
    | 'Tom' | 30 |
              ^
    """

  Scenario: syntax error too many cells
    Given the following dal code:
    """
    | name  | age |
    | 'Tom' |
    """
    Then failed to get "table" node with the following message:
    """
    Different cell size
    """
    And got the following source code information:
    """
    | name  | age |
    | 'Tom' |
             ^
    """

  Scenario: syntax error missing |
    Given the following dal code:
    """
    | name
    """
    Then failed to get "table" node with the following message:
    """
    Should end with `|`
    """
    And got the following source code information:
    """
    | name
          ^
    """

  Scenario: compile schema in header
    Given the following dal code:
    """
    | name is String |
    | 'Tom'          |
    """
    Then got the following "table" node:
    """
    : {
      inspect: "| name is String: |
    | is String: 'Tom' |"
      headers: [{
        property: {
          class.simpleName: 'SchemaExpression'
        }
      }]
      rows: [[{
        leftOperand.inspect: 'name is String'
        operator.class.simpleName: 'Matcher'
        rightOperand.inspect: "'Tom'"
        }]]
    }
    """

  Scenario: use schema in header
    When the following input data:
    """
    [{
      "name": "Tom",
      "age": 10
    }]
    """
    Then the following assertion should pass:
    """
    : | name is String |
      | 'Tom'          |
    """
    And assert by the following code:
    """
    : | age is String |
      | 10            |
    """
    Then failed with the following message:
    """
    Expecting age to match schema `String` but was not
    """
    And got the following source code information:
    """
    : | age is String |
               ^
      | 10            |
        ^
    ^^^^^^^^^^^^^^^^^^^
    """

  Scenario: skip row with | *** |
    When the following input data:
    """
    [{
      "name": "Tom",
      "age": 10
    }]
    """
    Then the following assertion should pass:
    """
    = | name | age | id |
      | ***             |
    """

  Scenario: assert parts of rows
    When the following input data:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "John",
      "age": 20
    },{
      "name": "Lily",
      "age": 15
    }]
    """
    Then the following assertion should pass:
    """
    = | name  | age |
      | 'Tom' | 10  |
      | ...         |
    """
    And the following assertion should pass:
    """
    = | name  | age |
      | ...         |
      | 'Lily' | 15 |
    """

  Scenario: should raise error when invalid table
    When assert by the following code:
    """
    = | name   | age |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
      | 'Lily' | 20  |
    """
    Then failed with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
    = | name   | age |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
        ^
      | 'Lily' | 20  |
    """
    When assert by the following code:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
    """
    Then failed with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
        ^
      | 'Tom'  | 10  |
      | ...          |
    """
    When assert by the following code:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
      | 'Tom'  | 10  |
    """
    Then failed with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
        ^
      | 'Tom'  | 10  |
    """

  Scenario: support sort list by header from a to z
    When the following input data:
    """
    [{
      "name": "Tom"
    },{
      "name": "John"
    }]
    """
    Then the following assertion should pass:
    """
    = | ↑ name |
      | 'John' |
      | 'Tom'  |
    """

  Scenario: support sort list by header from z to a
    When the following input data:
    """
    [{
      "name": "John"
    },{
      "name": "Tom"
    }]
    """
    Then the following assertion should pass:
    """
    = | ↓ name |
      | 'Tom'  |
      | 'John' |
    """

  Scenario: support sort list by multi headers before assertion
    When the following input data:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "John",
      "age": 10
    },{
      "name": "Tomas",
      "age": 20
    }]
    """
    Then the following assertion should pass:
    """
    = | ↑ name   | ↓↓ age |
      | 'Tomas'  | 20     |
      | 'John'   | 10     |
      | 'Tom'    | 10     |
    """

  Scenario: compile schema in cell
    Given the following dal code:
    """
    | name      |
    | is String |
    """
    Then got the following "table" node:
    """
    : {
      inspect: "| name: |
    | is String |"
      rows: [[{
        class.simpleName: 'SchemaExpression'
        inspect: 'name is String'
        }]]
    }
    """

  Scenario: compile schema in header and cell
    Given the following dal code:
    """
    | time is String           |
    | is Instant: {year: 2000} |
    """
    Then got the following "table" node:
    """
    : {
      inspect: "| time is String: |
    | is Instant: {year: 2000} |"
      rows: [[{
        class.simpleName: 'Expression'
        operator.class.simpleName: 'Matcher'
        leftOperand: {
          class.simpleName: 'SchemaExpression'
          inspect: 'time is String is Instant'
        }
        rightOperand: {
          class.simpleName: 'ObjectNode'
          inspect: "{year: 2000}"
        }
      }]]
    }
    """

  Scenario: assert schema in table and header and cell
    Given the following input data:
    """
    [{
      "time": "2000-01-01T00:00:00"
    }]
    """
    Then the following assertion should pass:
    """
    : | time                           |
      | is LocalDateTime: {year: 2000} |
    """

  Scenario: compile schema in row
    Given the following dal code:
    """
              | name   |
    is Person | 'Tom'  |
              | 'John' |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      rowSchemas: [{} null]
    }
    """

  Scenario: schema in row
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero {
        public int id = 0;
    }
    """
    And the following input data:
    """
      [{
        "id": 1,
        "name": 'Tom'
      }]
    """
    When assert by the following code:
    """
    :         | name  |
    is IdZero | 'Tom' |
    """
    Then failed with the following message:
    """
    Expecting [0] to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following source code information:
    """
    :         | name  |
    is IdZero | 'Tom' |
       ^
    ^^^^^^^^^^^^^^^^^^^
    """
    When the following input data:
    """
      [{
        "id": 0,
        "name": 'Tom'
      }]
    """
    Then the following assertion should pass:
    """
    :         | aliasOfName |
    is IdZero | 'Tom'       |
    """

#TODO schema in header alias in cell
#TODO schema in cell alias in cell sub object
#TODO schema for table alias in header and cell
#TODO table transpose
