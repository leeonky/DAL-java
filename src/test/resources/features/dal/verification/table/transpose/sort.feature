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
    = >>| ￪ name | 'John' | 'Tom'  |
    """
    And the inspect should:
    """
    = >>| ￪ name | name= 'John' | name= 'Tom' |
    """
    When evaluate by:
    """
    = >>| ￪ name | 'John' | 'Lucy'  |
    """
    And got the following notation:
    """
    = >>| ￪ name | 'John' | 'Lucy'  |
                            ^
                            ^
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
    = >>| ￬ name | 'Tom'  | 'John' |
    """
    And the inspect should:
    """
    = >>| ￬ name | name= 'Tom' | name= 'John' |
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
    = >>| ￪ name | 'Tomas' | 'John' | 'Tom' |
        | ￬￬ age | 20      | 10     | 10    |
    """
    And the inspect should:
    """
    = >>| ￪ name | name= 'Tomas' | name= 'John' | name= 'Tom' |
    | ￬￬ age | age= 20 | age= 10 | age= 10 |
    """
