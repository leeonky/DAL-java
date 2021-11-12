Feature: ignore some rows

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

  Scenario: specify index in row
    Given the following dal code:
    """
      | name   |
    0 | 'Tom'  |
    1 | 'John' |
    """
    Then got the following "table" node:
    """
    :{
      inspect: "| name |
    0 | : 'Tom' |
    1 | : 'John' |"
    }
    """

  Scenario: assert row with specified index
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
    0 | 'Tom' | 10  |
    2 | 'Lily' | 15 |
    """
    And the following assertion should pass:
    """
    =  | name   | age |
    -1 | 'Lily' | 15  |
    """

# TODO support specify index in table
# TODO syntax error when some row has index and some row not
# TODO support set first index of list
# TODO customized assertion in schema
