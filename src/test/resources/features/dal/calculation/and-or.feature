Feature: and-or

  Scenario Outline: boolean && and ,
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
      | true  | and      | true  | true   |
      | true  | and      | false | false  |
      | false | and      | true  | false  |
      | false | and      | false | false  |
      | true  | ,        | true  | true   |
      | true  | ,        | false | false  |
      | false | ,        | true  | false  |
      | false | ,        | false | false  |

  Scenario Outline: boolean ||
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
      | true  | \|\|     | false | true   |
      | false | \|\|     | true  | true   |
      | true  | \|\|     | true  | true   |
      | false | \|\|     | false | false  |
      | false | or       | true  | true   |
      | true  | or       | false | true   |
      | true  | or       | true  | true   |
      | false | or       | true  | true   |

  Scenario Outline: value && false means (null, false, number 0)
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

  Scenario Outline: value || false means (null, false, number 0)
    When evaluate by:
    """
      <input> <operator> <value>
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input   | operator | value   | result |
      | 'hello' | \|\|     | 'world' | hello  |
      | 'hello' | \|\|     | false   | hello  |
      | 'hello' | \|\|     | null    | hello  |
      | 'hello' | \|\|     | 0       | hello  |
      | false   | \|\|     | 'hello' | hello  |
      | null    | \|\|     | 'hello' | hello  |
      | 0       | \|\|     | 'hello' | hello  |
      | 0.0     | \|\|     | 'hello' | hello  |
      | 0.0BD   | \|\|     | 'hello' | hello  |
