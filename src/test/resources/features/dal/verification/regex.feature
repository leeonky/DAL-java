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
    And the inspect should:
    """
    'hello'<opt> /unmatched/
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