@NewTable
Feature: skip

  Scenario: skip row with | *** |
    When the following json:
    """
    [{
      "name": "Tom",
      "age": 10
    }]
    """
    Then the following verification should pass:
    """
    = | name | age | id |
      | ***             |
    """
    And the inspect should:
    """
    = | name | age | id |
    | *** |
    """

  Scenario: skip head or tail rows
    When the following json:
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
    Then the following verification should pass:
    """
    = | name  | age |
      | 'Tom' | 10  |
      | ...         |
    """
    And the inspect should:
    """
    = | name | age |
    | name= 'Tom' | age= 10 |
    | ... |
    """
    And the following verification should pass:
    """
    = | name  | age |
      | ...         |
      | 'Lily' | 15 |
    """
    And the inspect should:
    """
    = | name | age |
    | ... |
    | name= 'Lily' | age= 15 |
    """

  Scenario: specify index before row
    When the following json:
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
    Then the following verification should pass:
    """
    = | name  | age |
    0 | 'Tom' | 10  |
    2 | 'Lily' | 15 |
    """
    And the inspect should:
    """
    = | name | age |
    0 | name= 'Tom' | age= 10 |
    2 | name= 'Lily' | age= 15 |
    """
    And the following verification should pass:
    """
    =  | name   | age |
    -1 | 'Lily' | 15  |
    """
    And the inspect should:
    """
    = | name | age |
    -1 | name= 'Lily' | age= 15 |
    """
