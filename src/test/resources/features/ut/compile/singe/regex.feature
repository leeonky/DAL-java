Feature: regex node

  Scenario: return empty when first char is not /
    Given the following dal code xx:
    """
    not starts with /
    """
    Then got the following "regex" node xx:
    """
    : null
    """

  Scenario: compile regex
    Given the following dal code xx:
    """
     /hello/
    """
    Then got the following "regex" node xx:
    """
    : {
      class.simpleName: 'RegexNode'
      inspect: '/hello/'
      positionBegin: 1
    }
    """

  Scenario: escape chars (\/ => /)
    Given the following dal code xx:
    """
    /\//
    """
    Then got the following "regex" node xx:
    """
    inspect: '///'
    """

  Scenario Outline: syntax error: incomplete regex
    Given the following dal code xx:
    """
    <code>
    """
    Then failed to get "regex" node with the following message xx:
    """
    should end with `/`
    """
    And got the following source code information xx:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | /str |
      | /st\ |

  Scenario: regex does not match
    When assert by the following code xx:
    """
    'hello'= /unmatched/
    """
    Then failed with the following message xx:
    """
    expected ['hello'] matches /unmatched/ but was not
    """
    And got the following source code information xx:
    """
    'hello'= /unmatched/
             ^
    """

  Scenario: input value of 'equal to regex' must string type
    When assert by the following code xx:
    """
    100= /100/
    """
    Then failed with the following message xx:
    """
    Operator = before regex need a string input value
    """
    And got the following source code information xx:
    """
    100= /100/
       ^
    """

  Scenario: convert input value to string when 'match to regex'
    Then the following assertion should pass xx:
    """
    100: /100/
    """
