Feature: invoke

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
      <input> <operator> obj.value
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
      | 5     | =        | 5     | true   |
      | 3     | :        | 3.0   | true   |

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
      | input | operator | value | result |
      | 10    | +        | 1     | 11     |
#      | 10    | -        | 1     | 9      |
#      | 10    | *        | 2     | 20     |
#      | 10    | /        | 2     | 5      |
#      | true  | &&       | true  | true   |
#      | true  | \|\|     | false | true   |
#      | true  | and      | true  | true   |
#      | false | or       | true  | true   |
#      | 10    | >        | 5     | true   |
#      | 4     | <        | 4     | false  |
#      | 10    | >=       | 10    | true   |
#      | 5     | <=       | 4     | false  |
#      | 5     | !=       | 3     | true   |

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

#  TODO (list match)= ['value'], : ['value'], (property value)= (['value'])
