Feature: plus-sub

  Scenario Outline: plus precedence
    When evaluate by:
    """
      <input> <operator> <value> + 6
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <input> <operator> <value> + 6
    """
    Examples:
      | input | operator | value | result |
      | 10    | >        | 6     | false  |
      | 4     | <        | 3     | true   |
      | 10    | >=       | 8     | false  |
      | 5     | <=       | 4     | true   |
      | 6     | !=       | 0     | false  |

  Scenario Outline: plus parentheses with logic opt
    When evaluate by:
    """
    <input> <opt> 10+true
    """
    Then failed with the message:
    """
    Can not plus 'java.lang.Integer' and 'java.lang.Boolean'
    """
    Examples:
      | input | opt  |
      | true  | &&   |
      | true  | and  |
      | false | \|\| |
      | false | or   |

  Scenario: plus with which
    Given the following json:
    """
    {
      "number": 10,
      "operand": 5,
      "obj": {
        "number": 10,
        "operand": 2
      }
    }
    """
    Then the following verification should pass:
    """
    obj which number+operand
    """
    Then the result should:
    """
    : 12
    """

  Scenario Outline: plus with judgement
    * the following verification should pass:
    """
    4 <opt> 2+2
    """
    Examples:
      | opt |
      | :   |
      | =   |

  Scenario Outline: sub precedence
    When evaluate by:
    """
      <input> <operator> <value> - 10
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <input> <operator> <value> - 10
    """
    Examples:
      | input | operator | value | result |
      | 4     | >        | 6     | true   |
      | 4     | <        | 6     | false  |
      | 4     | >=       | 6     | true   |
      | 4     | <=       | 6     | false  |
      | 3     | !=       | 13    | false  |

  Scenario Outline: sub parentheses with logic opt
    When evaluate by:
    """
    <input> <opt> 10-true
    """
    Then failed with the message:
    """
    Operands should be number but 'java.lang.Integer' and 'java.lang.Boolean'
    """
    Examples:
      | input | opt  |
      | true  | &&   |
      | true  | and  |
      | false | \|\| |
      | false | or   |

  Scenario: sub with which
    Given the following json:
    """
    {
      "number": 10,
      "operand": 5,
      "obj": {
        "number": 10,
        "operand": 2
      }
    }
    """
    Then the following verification should pass:
    """
    obj which number-operand
    """
    Then the result should:
    """
    : 8
    """

  Scenario Outline: sub with judgement
    * the following verification should pass:
    """
    3 <opt> 4-1
    """
    Examples:
      | opt |
      | :   |
      | =   |
