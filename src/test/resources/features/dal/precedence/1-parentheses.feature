Feature: parentheses

  Scenario Outline: parentheses precedence
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
      <input> <operator> (obj which value)
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <input> <operator> (obj which value)
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

  Scenario Outline: verification before parentheses precedence
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
      <input> <operator> (obj which value)
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <input><operator> (obj which value)
    """
    Examples:
      | input | operator | value | result |
      | 5     | =        | 5     | 5      |
      | 3     | :        | 3.0   | 3      |

  Scenario: parentheses and which precedence
    Given the following json:
    """
      {
        "obj": {
          "value": {
            "number": 100
          }
        }
      }
    """
    Then the following verification should pass:
    """
    (obj which (value which number)) = ((obj which value) which number)
    """

  Scenario: parentheses and unary precedence
    When evaluate by:
    """
    (-(1+1))
    """
    Then the result should:
    """
    : -2
    """
    When evaluate by:
    """
    !(true || true)
    """
    Then the result should:
    """
    : false
    """

  Scenario: parentheses and is
    * the following verification should syntax error:
    """
    is (Schema)
    """
