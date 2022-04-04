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
      | opt  | input | result |
      | &&   | false | false  |
      | and  | false | false  |
      | \|\| | false | true   |
      | or   | false | true   |

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
      | opt  | input | result |
      | &&   | false | false  |
      | and  | false | false  |
      | \|\| | false | true   |
      | or   | false | true   |

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
      | :   | true   |
      | =   | true   |
