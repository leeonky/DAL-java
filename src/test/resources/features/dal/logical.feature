Feature: logic

  Scenario Outline: logic
    When evaluate by:
    """
      <input> <operator> <value>
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input | operator | value | result |
      | true  | &&       | true  | true   |
      | true  | &&       | false | false  |
      | false | &&       | true  | false  |
      | false | &&       | false | false  |
      | true  | \|\|     | false | true   |
      | false | \|\|     | true  | true   |
      | true  | \|\|     | true  | true   |
      | false | \|\|     | false | false  |
      | true  | and      | true  | true   |
      | true  | and      | false | false  |
      | false | and      | true  | false  |
      | false | and      | false | false  |
      | false | or       | true  | true   |
      | true  | or       | false | true   |
      | true  | or       | true  | true   |
      | false | or       | true  | true   |
      | 10    | >        | 5     | true   |
      | 10    | >        | 10    | false  |
      | 4     | <        | 10    | true   |
      | 4     | <        | 4     | false  |
      | 10    | >=       | 4     | true   |
      | 10    | >=       | 10    | true   |
      | 3     | >=       | 10    | false  |
      | 4     | <=       | 10    | true   |
      | 4     | <=       | 4     | true   |
      | 5     | <=       | 4     | false  |
      | 5     | !=       | 3     | true   |
      | 5     | !=       | 5     | false  |

  Scenario: support logic not
    When evaluate by:
    """
    !false
    """
    Then the result should:
    """
    =true
    """

  Scenario Outline: value logic false contains (null, false, number 0)
    When evaluate by:
    """
      <input> <operator> <value>
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input   | operator | value   | result  |
      | 'hello' | &&       | 'world' | 'world' |
      | 'hello' | &&       | false   | false   |
      | 'hello' | &&       | null    | null    |
      | 'hello' | &&       | 0       | 0       |
      | false   | &&       | 'hello' | false   |
      | null    | &&       | 'hello' | null    |
      | 0       | &&       | 'hello' | 0       |
      | 0.0     | &&       | 'hello' | 0.0     |
      | 0.0BD   | &&       | 'hello' | 0.0BD   |
      | 'hello' | \|\|     | 'world' | hello   |
      | 'hello' | \|\|     | false   | hello   |
      | 'hello' | \|\|     | null    | hello   |
      | 'hello' | \|\|     | 0       | hello   |
      | false   | \|\|     | 'hello' | hello   |
      | null    | \|\|     | 'hello' | hello   |
      | 0       | \|\|     | 'hello' | hello   |
      | 0.0     | \|\|     | 'hello' | hello   |
      | 0.0BD   | \|\|     | 'hello' | hello   |
