Feature: compile table node

  Scenario: support table with only header and header judgement operator was specified
    Given the following dal code:
    """
    | name: |
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| name: |'
    }
    """

  Scenario: support table with header and cells
    Given the following dal code:
    """
    | name: |
    | 'Tom' |
    | 'Ada' |
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: "| name: |
    | 'Tom' |
    | 'Ada' |"
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
    = | name= | age= |
      | 'Tom' | 10   |
    """
    And the following assertion should pass:
    """
    : | name= | age= |
      | 'Tom' | 10   |
    """
    When assert by the following code:
    """
    = | name= | age= |
      | 'Tom' | 11   |
    """
    Then failed with the following message:
    """
    Expecting java.lang.Integer
    <10>
    to be equal to java.lang.Integer
    <11>
    but was not
    """
    And got the following source code information:
    """
    = | name= | age= |
      | 'Tom' | 11   |
                ^
    """

#  TODO incorrect cell value
#  TODO incorrect size
#  TODO unexpected fields

#TODO default table judgement operator to header
#TODO default header judgement operator to cell
#TODO specify cell judgement operator
#TODO schema in header
#TODO schema in header alias in cell
#TODO schema in cell alias in cell sub object
#TODO schema for table alias in header and cell
#TODO sort in header
#TODO table transpose
