Feature: schema which expression

  Scenario: should raise error when no clause
    When evaluate by:
    """
      1 is Integer which
    """
    Then failed with the message:
    """
    expect a value or expression
    """
    And got the following notation:
    """
      1 is Integer which
                        ^
    """

  Scenario Outline: can omit which when clause start with = or :
    * the following verification should pass:
    """
    '1' is String <operator> '1'
    """
    Examples:
      | operator |
      | :        |
      | =        |
      | which =  |
      | which :  |

  Scenario: verify data matches schema and which clause
    Given the following schema class:
    """
    public class IdZero {
        public int id = 0;
    }
    """
    When the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is IdZero which .id=0
    """
    Then the following verification should pass:
    """
      is IdZero which id=0
    """
    Then the following verification should pass:
    """
      is IdZero which = {
        id: 0
      }
    """
    Then the following verification should pass:
    """
      is IdZero = {
        id: 0
      }
    """

#    TODO support field alias in multi schema list: is A / B
#    TODO support field alias in nested schema: is A is B
