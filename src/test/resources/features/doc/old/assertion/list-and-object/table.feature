Feature: table

  Scenario: support assert list object as table
    Given the following input data:
    """
      [{
        "name": "John",
        "age": 18
      }, {
        "name": "Tomas",
        "age": 21
      }]
    """
    Then the following assertion should pass:
    """
      = | name:   | age: |
        | 'John'  | 18   |
        | 'Tomas' | 21   |
    """

  Scenario: assert table with default verification operator
    Given the following input data:
    """
      [{
        "name": "John",
        "age": 18
      }, {
        "name": "Tomas",
        "age": 21
      }]
    """
    Then the following assertion should pass:
    """
      : | name=   | age: |
        | 'John'  | 18.0 |
        | /Tomas/ | 21.0 |
    """
    And the following assertion should pass:
    """
      : | name    | age |
        | 'John'  | 18  |
        | 'Tomas' | 21  |
    """
    And the following assertion should pass:
    """
      : | name    | ￬ age |
        | 'Tomas' | 21    |
        | 'John'  | 18    |
    """

  Scenario: support table transpose
    Given the following input data:
    """
      [{
        "name": "John",
        "age": 18
      }, {
        "name": "Tomas",
        "age": 21
      }]
    """
    Then the following assertion should pass:
    """
    = >>| name: | 'John' | 'Tomas' |
        | age:  | 18     | 21      |
    """
