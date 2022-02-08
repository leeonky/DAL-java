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

  Scenario: use previous value as current code scope in which clause
    * the following verification should pass:
    """
    'http://www.baidu.com' is URL which .host='www.baidu.com'
    """
    And the inspect should:
    """
    'http://www.baidu.com' is URL which .host= 'www.baidu.com'
    """
    But the following verification should failed:
    """
    'http://www.baidu.com' is URL which .host='www.google.com'
    """
    And got the following notation:
    """
    'http://www.baidu.com' is URL which .host='www.google.com'
                                              ^
    """

  Scenario: verify data use which clause by different styles
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
#TODO remove which :/=