Feature: basic verification via table

  Scenario: table one header and no rows
    Given the following json:
    """
    []
    """
    Then the following verification should pass:
    """
    : | name |
    """
    And the inspect should:
    """
    : | name |
    """
    Given the following json:
    """
    [1]
    """
    When evaluate by:
    """
    : | name |
    """
    Then failed with the message:
    """
    Expecting list size to be <0> but was <1>
    """
    And got the following notation:
    """
    : | name |
    ^
    """

  Scenario: table one header and one row
    Given the following json:
    """
    [{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    : | name  |
      | 'Tom' |
    """
    And the inspect should:
    """
    : | name |
    | name: 'Tom' |
    """
    Given the following json:
    """
    [{
      "name": "John"
    }]
    """
    When evaluate by:
    """
    : | name  |
      | 'Tom' |
    """
    Then failed with the message:
    """
    Expecting java.lang.String
    <John>
    to match java.lang.String
    <Tom>
    but was not
    """
    And got the following notation:
    """
    : | name  |
      | 'Tom' |
        ^
    ^^^^^^^^^^^
    """

  Scenario: judgement table by table judgement
    Given the following json:
    """
    [{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    = | name   |
      | 'Tom'  |
    """
    And the inspect should:
    """
    = | name |
    | name= 'Tom' |
    """
    Given the following json:
    """
    [{
      "name": "John",
      "age": 10
    }]
    """
    When evaluate by:
    """
    = | name   |
      | 'Tom'  |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in [0]
    """
    And got the following notation:
    """
    = | name   |
    ^
      | 'Tom'  |
    ^^^^^^^^^^^^
    """

  Scenario: compile table with row judgement operator which has higher priority than table judgement operator
    Given the following json:
    """
    [{
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    : | name   |
    = | 'Tom'  |
    """
    And the inspect should:
    """
    : | name |
    = | name= 'Tom' |
    """
    Given the following json:
    """
    [{
      "name": "John",
      "age": 10
    }]
    """
    When evaluate by:
    """
    : | name   |
    = | 'Tom'  |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in [0]
    """
    And got the following notation:
    """
    : | name   |
    = | 'Tom'  |
    ^
    ^^^^^^^^^^^^
    """

  Scenario: compile table and specified header judgement operator which has higher priority than row judgement operator
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | user=         |
    : | {name: 'Tom'} |
    """
    And the inspect should:
    """
    : | user= |
    : | user= {name: 'Tom'} |
    """
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom",
        "age": 10
      }
    }]
    """
    When evaluate by:
    """
    : | user=         |
    : | {name: 'Tom'} |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in user
    """
    And got the following notation:
    """
    : | user=         |
            ^
    : | {name: 'Tom'} |
    ^^^^^^^^^^^^^^^^^^^
    """

  Scenario: compile table and specified cell judgement operator which has higher priority than header judgement operator
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | user:           |
    : | = {name: 'Tom'} |
    """
    And the inspect should:
    """
    : | user: |
    : | user= {name: 'Tom'} |
    """
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom",
        "age": 10
      }
    }]
    """
    When evaluate by:
    """
    : | user:           |
    : | = {name: 'Tom'} |
    """
    Then failed with the message:
    """
    Unexpected fields `age` in user
    """
    And got the following notation:
    """
    : | user:           |
    : | = {name: 'Tom'} |
        ^
    ^^^^^^^^^^^^^^^^^^^^^
    """
