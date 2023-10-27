Feature: skip

  Scenario: skip one row
    When the following json:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "Lucy",
      "age": 13
    },{
      "name": "John",
      "age": 16
    }]
    """
    Then the following verification should pass:
    """
    = | name   | age |
      | 'Tom'  | 10  |
      | ***          |
      | 'John' | 16  |
    """
    And the inspect should:
    """
    = | name | age |
    | = 'Tom' | = 10 |
    | *** |
    | = 'John' | = 16 |
    """
    When evaluate by:
    """
    = | name   | age |
      | 'Tom'  | 10  |
      | ***          |
      | 'John' | 12  |
    """
    Then got the following notation:
    """
    = | name   | age |
      | 'Tom'  | 10  |
      | ***          |
      | 'John' | 12  |
                 ^
    ^^^^^^^^^^^^^^^^^^^
    """

  Scenario: skip row should not access any property
    When the following java class:
    """
    public class Data extends java.util.ArrayList {
      public Data() {
        add("any value");
      }
    }
    """
    Then use a instance of java class "Data" to evaluate:
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
    | = 'Tom' | = 10 |
    | ... |
    """
    When evaluate by:
    """
    = | name  | age |
      | 'Tom' | 12  |
      | ...         |
    """
    Then got the following notation:
    """
    = | name  | age |
      | 'Tom' | 12  |
                ^
    ^^^^^^^^^^^^^^^^^^
      | ...         |
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
    | = 'Lily' | = 15 |
    """
    When evaluate by:
    """
    = | name  | age |
      | ...         |
      | 'Lily' | 25 |
    """
    And got the following notation:
    """
    = | name  | age |
      | ...         |
      | 'Lily' | 25 |
                 ^
    ^^^^^^^^^^^^^^^^^^
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
    0 | = 'Tom' | = 10 |
    2 | = 'Lily' | = 15 |
    """
    When evaluate by:
    """
    = | name  | age |
    0 | 'Tom' | 12  |
    2 | 'Lily' | 15 |
    """
    Then got the following notation:
    """
    = | name  | age |
    0 | 'Tom' | 12  |
                ^
    ^^^^^^^^^^^^^^^^^^
    2 | 'Lily' | 15 |
    """
    When evaluate by:
    """
    = | name  | age |
    0 | 'Tom' | 10  |
    2 | 'Lily' | 25 |
    """
    And got the following notation:
    """
    = | name  | age |
    0 | 'Tom' | 10  |
    2 | 'Lily' | 25 |
                 ^
    ^^^^^^^^^^^^^^^^^^
    """
    And the following verification should pass:
    """
    =  | name   | age |
    -1 | 'Lily' | 15  |
    """
    And the inspect should:
    """
    = | name | age |
    -1 | = 'Lily' | = 15 |
    """
    Then the following verification should pass:
    """
    =  | name   | age |
     2 | 'Lily' | 15  |
    -2 | 'John' | 20  |
    """
    And the inspect should:
    """
    = | name | age |
    2 | = 'Lily' | = 15 |
    -2 | = 'John' | = 20 |
    """
    When evaluate by:
    """
    =  | name   | age |
     2 | 'Lily' | 15  |
    -2 | 'John' | 30  |
    """
    Then got the following notation:
    """
    =  | name   | age |
     2 | 'Lily' | 15  |
    -2 | 'John' | 30  |
                  ^
    ^^^^^^^^^^^^^^^^^^^^
    """
    When evaluate by:
    """
     = | name | age |
     3 | *    | *   |
    """
    Then got the following notation:
    """
     = | name | age |
     3 | *    | *   |
     ^
    ^^^^^^^^^^^^^^^^^^
    """

  Scenario: ignore row should not check row index
    Given the following json:
    """
    []
    """
    Then the following verification should pass:
    """
    = | name  | age |
    0 | ***         |
    """

  Scenario: contains element
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
      | ...         |
      | 'Lily' | 15 |
      | ...         |
    """
    When evaluate by:
    """
    = | name  | age |
      | ...         |
      | 'Lily' | 18 |
      | ...         |
    """
    Then failed with the message:
    """
    No such element
    """
    And got the following notation:
    """
    = | name  | age |
      | ...         |
      | 'Lily' | 18 |
    ^^^^^^^^^^^^^^^^^^
      | ...         |
    """

  Scenario: skip row should check size
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
     = | name | age | id |
       | ***             |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <1>
    Actual: <0>
    """
    And got the following notation:
    """
     = | name | age | id |
       ^
       | ***             |
    """
