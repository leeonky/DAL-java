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

  Scenario: raise error when some row has index and some row not
    Given the following dal code:
    """
      | name   |
    0 | 'Tom'  |
      | 'John' |
    """
    Then failed to get "table" node with the following message:
    """
    Row index should be consistent
    """
    And got the following source code information:
    """
      | name   |
    0 | 'Tom'  |
    ^^^^^^^^^^^^
      | 'John' |
    ^^^^^^^^^^^^
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
