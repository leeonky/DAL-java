Feature: compile table node

  Scenario: compile table header with only header and use table default judgement operator
    Given the following dal code:
    """
    | name |
    """
    Then got the following "judgement-expression-operand" node:
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
    Then got the following "judgement-expression-operand" node:
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
    Then got the following "judgement-expression-operand" node:
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

  Scenario: compile table and cell use cell judgement operator
    Given the following dal code:
    """
    | name=  |
    | :'Tom' |
    """
    Then got the following "judgement-expression-operand" node:
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
    }]
    """
    Then the following assertion should pass:
    """
    = | name  | age |
      | 'Tom' | 10  |
    """
    And the following assertion should pass:
    """
    : | name  | age  |
      | 'Tom' | 10.0 |
    """
    When assert by the following code:
    """
    = | name  | age  |
      | 'Tom' | 10.0 |
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
    = | name  | age  |
      | 'Tom' | 10.0 |
                ^
    """
    When assert by the following code:
    """
    = | name  |
      | 'Tom' |
    """
    Then failed with the following message:
    """
    Unexpected fields `age` in [0]
    """
    And got the following source code information:
#    TODO code position
    """
    = | name  |
    ^
      | 'Tom' |
    """

  Scenario: syntax error too many headers
    Given the following dal code:
    """
    | name  |
    | 'Tom' | 30 |
    """
    Then failed to get "judgement-expression-operand" node with the following message:
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
    Then failed to get "judgement-expression-operand" node with the following message:
    """
    Different cell size
    """
    And got the following source code information:
    """
    | name  | age |
    | 'Tom' |
             ^
    """

  Scenario: compile schema in header
    Given the following dal code:
    """
    | name is String |
    | 'Tom'          |
    """
    Then got the following "judgement-expression-operand" node:
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
#    TODO code position and message
    And got the following source code information:
    """
    : | age is String |
               ^
      | 10            |
        ^
    """

#  Scenario: compile schema in cell
#  Scenario: compile schema in header and cell

#TODO  Scenario: assert schema in table and header and cell

#TODO schema in header alias in cell
#TODO schema in cell alias in cell sub object
#TODO schema for table alias in header and cell
#TODO sort in header
#TODO table transpose
