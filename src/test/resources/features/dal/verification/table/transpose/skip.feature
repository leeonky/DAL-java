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
    = >>| name | 'Tom' | *** | 'John' |
        | age  | 10    |     | 16     |
    """
    And the inspect should:
    """
    = >>| name | = 'Tom' | *** | = 'John' |
    | age | = 10 |  | = 16 |
    """
    When evaluate by:
    """
    = >>| name | 'Tom' | *** | 'John' |
        | age  | 10    |     | 12     |
    """
    Then got the following notation:
    """
    = >>| name | 'Tom' | *** | 'John' |
                               ^
        | age  | 10    |     | 12     |
                               ^
                               ^
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
    = >>| name | *** |
        | age  |     |
        | id   |     |
    """
    And the inspect should:
    """
    = >>| name | *** |
    | age |  |
    | id |  |
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
    = >>| name | 'Tom' | ... |
        | age  | 10    |     |
    """
    And the inspect should:
    """
    = >>| name | = 'Tom' | ... |
    | age | = 10 |  |
    """
    When evaluate by:
    """
    = >>| name | 'Tom' | ... |
        | age  | 15    |     |
    """
    Then got the following notation:
    """
    = >>| name | 'Tom' | ... |
                 ^
        | age  | 15    |     |
                 ^
                 ^
    """
    And the following verification should pass:
    """
    = >>| name | ... | 'Lily' |
        | age  |     | 15     |
    """
    And the inspect should:
    """
    = >>| name | ... | = 'Lily' |
    | age |  | = 15 |
    """
    When evaluate by:
    """
    = >>| name | ... | 'Lily' |
        | age  |     | 25     |
    """
    And got the following notation:
    """
    = >>| name | ... | 'Lily' |
                       ^
        | age  |     | 25     |
                       ^
                       ^
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
    = | >>   | 0     | 2      |
      | name | 'Tom' | 'Lily' |
      | age  | 10    | 15     |
    """
    And the inspect should:
    """
    = | >> | 0 | 2 |
    | name | = 'Tom' | = 'Lily' |
    | age | = 10 | = 15 |
    """
    When evaluate by:
    """
    = | >>   | 0     | 2      |
      | name | 'Tom' | 'Lily' |
      | age  | 12    | 15     |
    """
    Then got the following notation:
    """
    = | >>   | 0     | 2      |
               ^
      | name | 'Tom' | 'Lily' |
               ^
      | age  | 12    | 15     |
               ^
               ^
    """
    When evaluate by:
    """
    = | >>   | 0     | 2      |
      | name | 'Tom' | 'Lily' |
      | age  | 10    | 25     |
    """
    Then got the following notation:
    """
    = | >>   | 0     | 2      |
                       ^
      | name | 'Tom' | 'Lily' |
                       ^
      | age  | 10    | 25     |
                       ^
                       ^
    """
    And the following verification should pass:
    """
    = | >>   | -1     |
      | name | 'Lily' |
      | age  | 15     |
    """
    And the inspect should:
    """
    = | >> | -1 |
    | name | = 'Lily' |
    | age | = 15 |
    """
    Then the following verification should pass:
    """
    = | >>   | 2      | -2     |
      | name | 'Lily' | 'John' |
      | age  |  15    | 20     |
    """
    And the inspect should:
    """
    = | >> | 2 | -2 |
    | name | = 'Lily' | = 'John' |
    | age | = 15 | = 20 |
    """
    When evaluate by:
    """
    = | >>   | 2      | -2     |
      | name | 'Lily' | 'John' |
      | age  |  15    | 30     |
    """
    Then got the following notation:
    """
    = | >>   | 2      | -2     |
                        ^
      | name | 'Lily' | 'John' |
                        ^
      | age  |  15    | 30     |
                        ^
                        ^
    """
    When evaluate by:
    """
    = | >>   | 3 |
      | name | * |
      | age  | * |
    """
    Then got the following notation:
    """
    = | >>   | 3 |
               ^
               ^
      | name | * |
               ^
      | age  | * |
               ^
    """

  Scenario: ignore row should not check row index
    Given the following json:
    """
    []
    """
    Then the following verification should pass:
    """
    = | >>   | 0   |
      | name | *** |
      | age  |     |
    """

  Scenario: list contains
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
    = >>| name | ... | 'Lily' | ... |
        | age  | ... | 15     |     |
    """
    When evaluate by:
    """
    = >>| name | ... | 'Lily' | ... |
        | age  | ... | 20     |     |
    """
    Then failed with the message:
    """
    No such element
    """
    And got the following notation:
    """
    = >>| name | ... | 'Lily' | ... |
                       ^
        | age  | ... | 20     |     |
                       ^
    """

  Scenario: skip row should check size
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
     = >>| name | *** |
         | age  |     |
         | id   |     |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <1>
    Actual: <0>
    """
    And got the following notation:
    """
     = >>| name | *** |
       ^
         | age  |     |
         | id   |     |
    """
