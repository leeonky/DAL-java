Feature: sort

  Scenario: support sort list by header from a to z
    When the following json:
    """
    [{
      "name": "Tom"
    },{
      "name": "John"
    }]
    """
    Then the following verification should pass:
    """
    = | ￪ name |
      | 'John' |
      | 'Tom'  |
    """
    And the inspect should:
    """
    = | ￪ name |
    | name= 'John' |
    | name= 'Tom' |
    """

  Scenario: support sort list by header from z to a
    When the following json:
    """
    [{
      "name": "John"
    },{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    = | ￬ name |
      | 'Tom'  |
      | 'John' |
    """
    And the inspect should:
    """
    = | ￬ name |
    | name= 'Tom' |
    | name= 'John' |
    """

  Scenario: support sort list by multi headers before assertion
    When the following json:
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
    Then the following verification should pass:
    """
    = | ￪ name   | ￬￬ age |
      | 'Tomas'  | 20     |
      | 'John'   | 10     |
      | 'Tom'    | 10     |
    """
    And the inspect should:
    """
    = | ￪ name | ￬￬ age |
    | name= 'Tomas' | age= 20 |
    | name= 'John' | age= 10 |
    | name= 'Tom' | age= 10 |
    """
