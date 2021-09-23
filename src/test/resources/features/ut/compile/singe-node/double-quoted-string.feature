Feature: "string"

  Scenario: return empty when first char is not matched
    Given the following dal code xx:
    """
    not starts with "
    """
    Then got the following "double-quoted-string" node xx:
    """
    : null
    """

  Scenario Outline: double quoted string
    Given the following dal code xx:
    """
     "<str>"
    """
    Then got the following "double-quoted-string" node xx:
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
    Given the following dal code xx:
    """
    "\\\n\t\""
    """
    And evaluate as "double-quoted-string" result is:
    """
    : '\\
    	"'
    """

  Scenario: keep original char when not valid escape char
    Given the following dal code xx:
    """
    "\h"
    """
    And evaluate as "double-quoted-string" result is:
    """
    : "\\h"
    """

  Scenario Outline: syntax error: incomplete string
    Given the following dal code xx:
    """
    <code>
    """
    Then failed to get "double-quoted-string" node with the following message xx:
    """
    should end with `"`
    """
    And got the following source code information xx:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | "str |
      | "st\ |
