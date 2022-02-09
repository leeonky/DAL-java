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
