Feature: 'string'

  Scenario: null when does not match
    Given the following dal expression:
    """
    not starts with '
    """
    Then parse the following "single-quoted-string" node:
    """
    : null
    """

  Scenario Outline: single quoted string
    Given the following dal expression:
    """
     '<str>'
    """
    Then parse the following "expression" node:
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
    When evaluate follow expression as "expression" node:
    """
    '\\\''
    """
    Then the result should:
    """
    : "\\'"
    """

  Scenario: keep original char when not valid escape char
    When evaluate follow expression as "expression" node:
    """
    '\h'
    """
    Then the result should:
    """
    : "\\h"
    """

  Scenario Outline: syntax error: incomplete string
    Given the following dal expression:
    """
    <code>
    """
    Then failed to parse "expression" with the following message:
    """
    should end with `'`
    """
    And got the following notation:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | 'str |
      | 'st\ |