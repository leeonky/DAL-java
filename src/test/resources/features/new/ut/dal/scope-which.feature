Feature: which expression

  Scenario: which opt should given clause new block scope
    Given the following java class:
    """
    public class Data {
      public java.time.LocalDateTime time = java.time.LocalDateTime.parse("2000-12-11T10:00:00");
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    time which year= 2000
    """

  Scenario: should raise error when no clause
    When evaluate by:
    """
      which
    """
    Then failed with the message:
    """
    expect a value or expression
    """
    And got the following notation:
    """
      which
           ^
    """

  Scenario Outline: should raise error when unexpected token
    When evaluate by:
    """
      which <code>
    """
    Then failed with the message:
    """
    expect a value or expression
    """
    And got the following notation:
    """
      which <code>
            ^
    """
    Examples:
      | code |
      | +    |
      | :    |
      | =    |

#      TODO to be removed
  Scenario Outline: can omit which when clause start with = or :
    * the following verification should pass:
    """
    '1' is String <operator> '1'
    """
    Examples:
      | operator |
      | :        |
      | =        |

#      TODO to be removed
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

#      TODO to be removed
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
      is IdZero = {
        id: 0
      }
    """

#    TODO support field alias in multi schema list: is A / B
#    TODO support field alias in nested schema: is A is B
#TODO remove which :/=
