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
