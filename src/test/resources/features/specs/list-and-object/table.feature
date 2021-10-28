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

  Scenario: assert table with default judgement operator
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
      : | name    | age |
        | 'John'  | 18  |
        | 'Tomas' | 21  |
    """

#  TODO default judgement operator in header
#  TODO sort list by header