Feature: comparison operators

  Scenario Outline: > with logical opt
    * the following verification should pass:
    """
    true <opt> 5>1
    """
    Examples:
      | opt  |
      | &&   |
      | and  |
      | \|\| |
      | or   |

  Scenario Outline: >= with logical opt
    * the following verification should pass:
    """
    true <opt> 5>1
    """
    Examples:
      | opt  |
      | &&   |
      | and  |
      | \|\| |
      | or   |

  Scenario Outline: < with logical opt
    * the following verification should pass:
    """
    true <opt> 1<5
    """
    Examples:
      | opt  |
      | &&   |
      | and  |
      | \|\| |
      | or   |

  Scenario Outline: <= with logical opt
    * the following verification should pass:
    """
    true <opt> 1<=5
    """
    Examples:
      | opt  |
      | &&   |
      | and  |
      | \|\| |
      | or   |

  Scenario Outline: with which
    Given the following json:
    """
    {
      "operand": 50,
      "obj": {
        "number": 10,
        "operand": 2
      }
    }
    """
    When evaluate by:
    """
    obj which number<opt>operand
    """
    Then the result should:
    """
    : <result>
    """
    Examples:
      | opt | result |
      | >   | true   |
      | >=  | true   |
      | <   | false  |
      | <=  | false  |
