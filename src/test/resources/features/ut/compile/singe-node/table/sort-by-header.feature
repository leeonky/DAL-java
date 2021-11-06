Feature: sort list in table

  Scenario: compile table header with sort constructors
    Given the following dal code:
    """
    | ++name | -age |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| ++ name | - age |'
      headers: | property.inspect | sequence.value | sequence.type |
               | 'name'           | 2              | 'AZ'          |
               | 'age'            | 1              | 'ZA'          |
    }
    """

  Scenario: compile table header with another style of sort constructors
    Given the following dal code:
    """
    | ↑↑ name | ↓ age |
    """
    Then got the following "table" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| ↑↑ name | ↓ age |'
      headers: | property.inspect | sequence.value | sequence.type |
               | 'name'           | 2              | 'AZ'          |
               | 'age'            | 1              | 'ZA'          |
    }
    """

  Scenario: support sort list by header from a to z
    When the following input data:
    """
    [{
      "name": "Tom"
    },{
      "name": "John"
    }]
    """
    Then the following assertion should pass:
    """
    = | ↑ name |
      | 'John' |
      | 'Tom'  |
    """

  Scenario: support sort list by header from z to a
    When the following input data:
    """
    [{
      "name": "John"
    },{
      "name": "Tom"
    }]
    """
    Then the following assertion should pass:
    """
    = | ↓ name |
      | 'Tom'  |
      | 'John' |
    """

  Scenario: support sort list by multi headers before assertion
    When the following input data:
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
    Then the following assertion should pass:
    """
    = | ↑ name   | ↓↓ age |
      | 'Tomas'  | 20     |
      | 'John'   | 10     |
      | 'Tom'    | 10     |
    """
