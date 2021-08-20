Feature: "string" token

  Scenario: return empty when first char is not matched
    Given the follow dal code:
    """
    not starts with "
    """
    Then got the following "double-quoted-string" token:
    """
    : null
    """

  Scenario Outline: parse string and seek code to the next char
    Given the follow dal code:
    """
    "<str>"=
    """
    Then got the following "double-quoted-string" token:
    """
    : {
      type: 'CONST_VALUE'
      value: '<str>'
    }
    """
    And current offset char of source code is "="
    Examples:
      | str   |
      |       |
      | hello |

  Scenario: parse string at the end of code
    Given the follow dal code:
    """
    "hello"
    """
    Then got the following "double-quoted-string" token:
    """
    : {
      type: 'CONST_VALUE'
      value: 'hello'
    }
    """

  Scenario: escape char
    Given the follow dal code:
    """
    "\\\n\t\""
    """
    Then got the following "double-quoted-string" token:
    """
    value= '\\
    	"'
    """

  Scenario: keep original char when not valid escape char
    Given the follow dal code:
    """
    "\h"
    """
    Then got the following "double-quoted-string" token:
    """
    value= '\h'
    """

  Scenario: syntax error: incomplete string
    Given the follow dal code:
    """
    "str
    """
    Then failed to take "double-quoted-string" token with the following message:
    """
    string should end with `"`
    """
    And got the following source code information:
    """
    "str
        ^
    """

  Scenario: syntax error: incomplete escape char string
    Given the follow dal code:
    """
    "str\
    """
    Then failed to take "double-quoted-string" token with the following message:
    """
    string should end with `"`
    """
    And got the following source code information:
    """
    "str\
         ^
    """
