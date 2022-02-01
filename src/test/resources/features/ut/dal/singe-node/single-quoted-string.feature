Feature: 'string'

  Scenario: null when does not match
    Given the following dal code:
    """
    not starts with quote
    """
    Then got the following "single-quoted-string" node:
    """
    : null
    """

  Scenario Outline: single quoted string
    Given the following dal code:
    """
     '<str>'
    """
    Then got the following "single-quoted-string" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '<inspect>'
      positionBegin: 1
    }
    """
    Examples:
      | str         | inspect         |
      |             | \'\'            |
      | hello world | \'hello world\' |

  Scenario: escape char
    Given the following dal code:
    """
    '\\\''
    """
    And node evaluate as "single-quoted-string" result is:
    """
    : "\\'"
    """

  Scenario: keep original char when not valid escape char
    Given the following dal code:
    """
    '\h'
    """
    And node evaluate as "single-quoted-string" result is:
    """
    : "\\h"
    """

  Scenario Outline: syntax error: incomplete string
    Given the following dal code:
    """
    <code>
    """
    Then failed to get "single-quoted-string" node with the following message:
    """
    should end with `'`
    """
    And got the following source code information:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | 'str |
      | 'st\ |
