Feature: regex node

  Scenario: return empty when first char is not /
    Given the following dal expression:
    """
    not starts with /
    """
    Then parse the following "regex" node:
    """
    : null
    """

  Scenario: compile regex
    Given the following dal expression:
    """
     /hello/
    """
    Then parse the following "regex" node:
    """
    : {
      class.simpleName: 'RegexNode'
      inspect: '/hello/'
      positionBegin: 1
    }
    """

  Scenario: escape chars (\/ => /)
    Given the following dal expression:
    """
    /\//
    """
    Then parse the following "regex" node:
    """
    inspect: '///'
    """

  Scenario Outline: syntax error: incomplete regex
    Given the following dal expression:
    """
    <code>
    """
    Then failed to parse "regex" with the following message:
    """
    should end with `/`
    """
    And got the following notation:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | /str |
      | /st\ |

#    TODO remove regex node use e2e test