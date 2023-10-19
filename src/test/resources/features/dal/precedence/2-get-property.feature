Feature: property

  Scenario Outline: dot property precedence
    Given the following json:
    """
      {
        "obj": {
          "value": <value>
        }
      }
    """
    When evaluate by:
    """
      <input> <operator> .obj.value
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input | operator | value | result |
      | 10    | +        | 1     | 11     |
      | 10    | -        | 1     | 9      |
      | 10    | *        | 2     | 20     |
      | 10    | /        | 2     | 5      |
      | true  | &&       | true  | true   |
      | true  | \|\|     | false | true   |
      | true  | and      | true  | true   |
      | false | or       | true  | true   |
      | 10    | >        | 5     | true   |
      | 4     | <        | 4     | false  |
      | 10    | >=       | 10    | true   |
      | 5     | <=       | 4     | false  |
      | 5     | !=       | 3     | true   |
      | 5     | =        | 5     | 5      |
      | 3     | :        | 3.0   | 3      |

  Scenario: which precedence
    Given the following json:
    """
      {
        "container": {
          "obj": {
            "value": 100
          }
        }
      }
    """
    Then the following verification should pass:
    """
    container which obj.value = 100
    """

  Scenario Outline: bracket precedence
    Given the following json:
    """
      {
        "obj": {
          "value": <value>
        }
      }
    """
    When evaluate by:
    """
      <input> <operator> obj['value']
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input | operator | value   | result  |
      | 10    | +        | 1       | 11      |
      | 10    | -        | 1       | 9       |
      | 10    | *        | 2       | 20      |
      | 10    | /        | 2       | 5       |
      | true  | &&       | true    | true    |
      | true  | \|\|     | false   | true    |
      | true  | and      | true    | true    |
      | false | or       | true    | true    |
      | 10    | >        | 5       | true    |
      | 4     | <        | 4       | false   |
      | 10    | >=       | 10      | true    |
      | 5     | <=       | 4       | false   |
      | 5     | !=       | 3       | true    |
      |       | which    | "input" | 'input' |

  Scenario:
  dot operator has higher precedence over unary operator(property chain after unary operator)
  unary operator has higher precedence over other operators
    Given the following json:
    """
      {
        "result": {
          "integer": 100,
          "boolean": true
        }
      }
    """
    When evaluate by:
    """
    1 + -result.integer
    """
    Then the result should:
    """
      : -99
    """
    When evaluate by:
    """
    true && !result.boolean
    """
    Then the result should:
    """
      : false
    """

  Scenario: [] after verification is list verification
    Given the following json:
    """
    ["a"]
    """
    Then the following verification should pass:
    """
    = ['a']
    """
    And the inspect should:
    """
    = [[0]= 'a']
    """
    Given the following json:
    """
    {
      "name": "Tom"
    }
    """
    Then the following verification should pass:
    """
    'Tom': (['name'])
    """
