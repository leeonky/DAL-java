Feature: regex node

  Scenario: return empty when first char is not /
    Given the following dal code:
    """
    not starts with /
    """
    Then got the following "regex" node:
    """
    : null
    """

  Scenario: compile regex
    Given the following dal code:
    """
     /hello/
    """
    Then got the following "regex" node:
    """
    : {
      class.simpleName: 'RegexNode'
      inspect: '/hello/'
      positionBegin: 1
    }
    """

  Scenario: escape chars (\/ => /)
    Given the following dal code:
    """
    /\//
    """
    Then got the following "regex" node:
    """
    inspect: '///'
    """

  Scenario Outline: syntax error: incomplete regex
    Given the following dal code:
    """
    <code>
    """
    Then failed to get "regex" node with the following message:
    """
    should end with `/`
    """
    And got the following source code information:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | /str |
      | /st\ |

  Scenario: regex does not match
    When assert by the following code:
    """
    'hello'= /unmatched/
    """
    Then failed with the following message:
    """
    expected java.lang.String
    <'hello'>
    matches /unmatched/ but was not
    """
    And got the following source code information:
    """
    'hello'= /unmatched/
             ^
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

  Scenario: convert input value to string when 'match to regex'
    Then the following assertion should pass:
    """
    100: /100/
    """
