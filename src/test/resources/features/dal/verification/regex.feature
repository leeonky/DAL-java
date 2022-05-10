Feature: regex

  Scenario: regex does not match via =
    When evaluate by:
    """
    'hello'= /unmatched/
    """
    Then failed with the message:
    """
    Expected to match: /unmatched/
    Actual: <hello>
    """
    And got the following notation:
    """
    'hello'= /unmatched/
             ^
    """
    And the inspect should:
    """
    'hello'= /unmatched/
    """

  Scenario: regex does not match via :
    When evaluate by:
    """
    123: /unmatched/
    """
    Then failed with the message:
    """
    Expected to match: /unmatched/
    Actual: <123> converted from: java.lang.Integer
    <123>
    """
    And got the following notation:
    """
    123: /unmatched/
         ^
    """
    And the inspect should:
    """
    123: /unmatched/
    """

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

  Scenario Outline: syntax error: incomplete regex
    When evaluate by:
    """
    : <code>
    """
    Then failed with the message:
    """
    Should end with `/`
    """
    And got the following notation:
    """
    : <code>
          ^
    """
    Examples:
      | code |
      | /str |
      | /st\ |

  Scenario: escape chars (\/ => /)
    * the following verification should pass:
    """
    "/": /\//
    """
    And the inspect should:
    """
    '/': ///
    """

  Scenario: return empty when first char is not /
    Given the following dal expression:
    """
    not starts with /
    """
    Then parse the following "regex" node:
    """
    : null
    """
