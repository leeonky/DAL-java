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
    Then failed to get the following "regex" node with the following message xx:
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
