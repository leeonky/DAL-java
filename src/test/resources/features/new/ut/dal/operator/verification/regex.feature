Feature: regex

  Scenario Outline: regex does not match
    When evaluate by:
    """
    'hello'<opt> /unmatched/
    """
    Then failed with the message:
    """
    Expecting java.lang.String
    <hello>
    to match /unmatched/ but was not
    """
    And got the following notation:
    """
    'hello'<opt> /unmatched/
             ^
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: input value of 'equal to regex' must string type
    When assert by the following code:
    """
    100= /100/
    """
    Then failed with the following message:
    """
    Operator = before regex need a string input value
    """
    And got the following source code information:
    """
    100= /100/
       ^
    """

  Scenario Outline: convert input value to string when 'match to regex'
    Then the following assertion should pass:
    """
    <input><opt> <regex>
    """
    Examples:
      | input | opt | regex |
      | '100' | =   | /100/ |
      | '100' | :   | /100/ |
      | 100   | :   | /100/ |
