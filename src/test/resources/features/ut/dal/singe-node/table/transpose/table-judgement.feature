Feature: compile table with judgement

  Scenario: compile table with only header and no rows
    Given the following dal code:
    """
    >>| name |
      | age  |
    """
    Then got the following "table" node:
    """
    inspect: '>>| name |
    | age |'
    """

  Scenario: compile table with only default table judgement operator
    Given the following dal code:
    """
    = >>| name | 'Tom' |
        | age  | 10    |
    """
    Then got the following "expression" node:
    """
    rightOperand: {
      class.simpleName: 'TableNodeBk'
      inspect: ">>| name | name= 'Tom' |
    | age | age= 10 |"
    }
    """

  Scenario: compile table with row judgement operator which has higher priority than table judgement operator
    Given the following dal code:
    """
    : | >>     | =     |
      | name   | 'Tom' |
    """
    Then got the following "expression" node:
    """
    rightOperand.inspect: "| >> | = |
    | name | name= 'Tom' |"
    """

  Scenario: compile table and specified header judgement operator which has higher priority than row judgement operator
    Given the following dal code:
    """
    | >>    | =     |
    | name: | 'Tom' |
    """
    Then got the following "table" node:
    """
    inspect: "| >> | = |
    | name: | name: 'Tom' |"
    """

  Scenario: compile table and specified cell judgement operator which has higher priority than header judgement operator
    Given the following dal code:
    """
    >>| name= | : 'Tom' |
    """
    Then got the following "table" node:
    """
    inspect: ">>| name= | name: 'Tom' |"
    """

  Scenario: judgement table with empty list
    Given the following input data:
    """
    [ 1 ]
    """
    When assert by the following code:
    """
    = >>| name |
    """
    Then failed with the following message:
    """
    Expecting list size to be <0> but was <1>
    """
    And got the following source code information:
    """
    = >>| name |
    ^
    """
    When the following input data:
    """
    []
    """
    Then the following assertion should pass:
    """
    = >>| name |
    """

  Scenario: judgement table by table judgement
    Given the following input data:
    """
    [{
      "id": "001",
      "amount": 10.0
    }]
    """
    When assert by the following code:
    """
    = >>| id     | '001' |
        | amount | 10    |
    """
    Then failed with the following message:
    """
    Expecting java.lang.Double
    <10.0>
    to be equal to java.lang.Integer
    <10>
    but was not
    """
    And got the following source code information:
    """
    = >>| id     | '001' |
                   ^
        | amount | 10    |
                   ^
                   ^
    """
    And the following assertion should pass:
    """
    : >>| id     | '001' |
    | amount | 10    |
    """

  Scenario: judgement table by row judgement operator
    Given the following input data:
    """
    [{
      "name": "Tom",
      "age": 10
    },{
      "name": "Lucy",
      "age": 10,
      "id": 1
    }]
    """
    When assert by the following code:
    """
    : | >>   |       | =      |
      | name | 'Tom' | 'Lucy' |
      | age  | 10.0  | 10.0   |
    """
    Then failed with the following message:
    """
    Unexpected fields `id` in [1]
    """
    And got the following source code information:
    """
    : | >>   |       | =      |
                       ^
      | name | 'Tom' | 'Lucy' |
                       ^
      | age  | 10.0  | 10.0   |
                       ^
    """
    And the following assertion should pass:
    """
    = | >>   |       | :      |
      | name | 'Tom' | 'Lucy' |
      | age  | 10    | 10     |
    """

  Scenario: judgement table by specified header judgement
    Given the following input data:
    """
    [{
      "amount": 18.0
    }]
    """
    And the following assertion should pass:
    """
    = | >>      | =  |
      | amount: | 18 |
    """

  Scenario: judgement table by specified cell judgement
    Given the following input data:
    """
    [{
      "amount": 18.0
    }]
    """
    And the following assertion should pass:
    """
    : >>| amount= | : 18 |
    """
