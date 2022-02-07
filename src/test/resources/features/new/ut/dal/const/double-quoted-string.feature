Feature: "string"

  Scenario Outline: double quoted string
    When evaluate by:
    """
     "<str>"
    """
    Then the result should:
    """
    : <value>
    """
    Examples:
      | str         | value         |
      |             | ''            |
      | hello world | 'hello world' |

  Scenario: escape char
    When evaluate by:
    """
    "\\\n\t\""
    """
    Then the result should:
    """
    : '\\
    	"'
    """

  Scenario: keep original char when not valid escape char
    When evaluate by:
    """
    "\h"
    """
    Then the result should:
    """
    : "\\h"
    """

  Scenario: record string position
    When evaluate by:
    """
    null: "hello"
    """
    Then got the following notation:
    """
    null: "hello"
          ^
    """

  Scenario Outline: syntax error: incomplete string
    When evaluate by:
    """
    <code>
    """
    Then failed with the message:
    """
    should end with `"`
    """
    And got the following notation:
    """
    <code>
        ^
    """
    Examples:
      | code |
      | "str |
      | "st\ |
