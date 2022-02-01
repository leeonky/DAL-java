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
    = >>| name | *** |
        | age  |     |
        | id   |     |
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
    = >>| name | 'Tom' | ... |
        | age  | 10    |     |
    """
    And the following assertion should pass:
    """
    = >>| name | ... | 'Lily' |
        | age  |     | 15     |
    """

  Scenario: specify index in row
    Given the following dal code:
    """
    | >>   | 0     | 1      |
    | name | 'Tom' | 'John' |
    """
    Then got the following "table" node:
    """
    :{
      inspect: "| >> | 0 | 1 |
    | name | : 'Tom' | : 'John' |"
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
    = | >>   | 0     | 2      |
      | name | 'Tom' | 'Lily' |
      | age  | 10    | 15     |
    """
    And the following assertion should pass:
    """
    = | >>   | -1     |
      | name | 'Lily' |
      | age  | 15     |
    """

  Scenario: raise error when some row has index and some row not
    Given the following dal code:
    """
    | >>   |       | 1      | 2      |
    | name | 'Tom' | 'John' | 'Lily' |
    | age  | 10    | 15     | 20     |
    """
    Then failed to get "table" node with the following message:
    """
    Row index should be consistent
    """
    And got the following source code information:
    """
    | >>   |       | 1      | 2      |
    | name | 'Tom' | 'John' | 'Lily' |
             ^
                     ^
    | age  | 10    | 15     | 20     |
             ^
                     ^
    """
