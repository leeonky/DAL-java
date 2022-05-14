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
    Expect a value or expression
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
    Expect a value or expression
    """
    And got the following notation:
    """
      which <code>
            ^
    """
    Examples:
      | code |
      | *    |
      | :    |
      | =    |

  Scenario: use previous value as current code scope in which clause
    When the following json:
    """
    {
      "obj": {
        "key": "123"
      }
    }
    """
    When evaluate by:
    """
    obj which key
    """
    Then the result should:
    """
    : '123'
    """
    And the following verification should pass:
    """
    obj which key: '123'
    """

#    TODO support field alias in multi schema list: is A / B
#    TODO support field alias in nested schema: is A is B
