Feature: "string"

  Scenario: return empty when first char is not matched
    Given the following dal code:
    """
    not starts with "
    """
    Then got the following "double-quoted-string" node:
    """
    : null
    """

  Scenario Outline: double quoted string
    Given the following dal code:
    """
     "<str>"
    """
    Then got the following "double-quoted-string" node:
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
    "\\\n\t\""
    """
    And evaluate as "double-quoted-string" result is:
    """
    : '\\
    	"'
    """

  Scenario: keep original char when not valid escape char
    Given the following dal code:
    """
    "\h"
    """
    And evaluate as "double-quoted-string" result is:
    """
    : "\\h"
    """

  Scenario Outline: syntax error: incomplete string
    Given the following dal code:
    """
    <code>
    """
    Then failed to get "double-quoted-string" node with the following message:
    """
    should end with `"`
    """
    And got the following source code information:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | "str |
      | "st\ |
