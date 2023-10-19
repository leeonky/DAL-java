Feature: verification operators

  Scenario Outline: : with logical opt
    When evaluate by:
    """
    <input> <opt> 5:5
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input | opt  | result |
      | false | &&   | false  |
      | true  | and  | 5      |
      | 0     | and  | 0      |
      | null  | and  | null   |
      | false | \|\| | 5      |
      | false | or   | 5      |
      | true  | or   | true   |
      | 100   | or   | 100    |

  Scenario Outline: = with logical opt
    When evaluate by:
    """
    <input> <opt> 5=5
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | input | opt  | result |
      | false | &&   | false  |
      | true  | and  | 5      |
      | 0     | and  | 0      |
      | null  | and  | null   |
      | false | \|\| | 5      |
      | false | or   | 5      |
      | true  | or   | true   |
      | 100   | or   | 100    |

  Scenario Outline: with which
    Given the following json:
    """
    {
      "operand": 50,
      "obj": {
        "number": 10,
        "operand": 10
      }
    }
    """
    When evaluate by:
    """
    obj which number<opt> .operand
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | opt | result |
      | :   | 10     |
      | =   | 10     |
