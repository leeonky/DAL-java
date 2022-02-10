Feature: logical

  Scenario Outline: logical and with which
    Given the following json:
    """
    {
      "operand": false,
      "obj": {
        "number": true,
        "operand": true
      }
    }
    """
    When evaluate by:
    """
    obj which number <opt> operand
    """
    Then the result should:
    """
    : true
    """
    Examples:
      | opt |
      | &&  |
      | and |

  Scenario Outline: logical or with which
    Given the following json:
    """
    {
      "operand": true,
      "obj": {
        "number": false,
        "operand": false
      }
    }
    """
    When evaluate by:
    """
    obj which number <opt> operand
    """
    Then the result should:
    """
    : false
    """
    Examples:
      | opt  |
      | \|\| |
      | or   |
