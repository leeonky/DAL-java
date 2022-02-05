Feature: invoke

  Scenario Outline: access property of root input object
    Given the following json:
    """
      {
        "name": "Tom"
      }
    """
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = 'Tom'
    """
    Examples:
      | code     |
      | name     |
      | .name    |
      | ['name'] |
      | ["name"] |

  Scenario: access input list
    Given the following json:
    """
      [1, 2, 3]
    """
    When evaluate by:
    """
      [0]
    """
    Then the result should:
    """
    : 1
    """

  Scenario: property chain
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }]
      }
    """
    When evaluate by:
    """
      items[0].id
    """
    Then the result should:
    """
    : 100
    """

  Scenario Outline: dot property precedence
    Given the following json:
    """
      {
        "value": <value>
      }
    """
    When evaluate by:
    """
      <input> <operator> .value
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
        "value": <value>
      }
    """
    When evaluate by:
    """
      <input> <operator> ['value']
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

  Scenario: do not allow empty property
    When evaluate by:
    """
    .
    """
    Then failed with the message:
    """
    expect a symbol
    """
    And got the following notation:
    """
    .
     ^
    """

  Scenario: do not allow empty property(has white space)
    When evaluate by:
    """
    . 
    """
    Then failed with the message:
    """
    expect a symbol
    """
    And got the following notation:
    """
    . 
      ^
    """

#  TODO opt is
#  TODO (list match)= ['value'], : ['value'], (property value)= (['value'])
#  TODO access list last index
