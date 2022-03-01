Feature: 'string'

  Scenario Outline: single quoted string
    When evaluate by:
    """
     '<str>'
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | str         | value         | inspect       |
      |             | ''            | ''            |
      | hello world | 'hello world' | 'hello world' |

  Scenario: single quoted string with blank
    When evaluate by:
    """
     '  '
    """
    Then the result should:
    """
    : '  ' and length= 2
    """
    And the inspect should:
    """
    '  '
    """

  Scenario: escape char
    When evaluate by:
    """
    '\\\''
    """
    Then the result should:
    """
    : "\\'"
    """

  Scenario: keep original char when not valid escape char
    When evaluate by:
    """
    '\h'
    """
    Then the result should:
    """
    : "\\h"
    """

  Scenario Outline: syntax error: incomplete string
    Given evaluate by:
    """
    <code>
    """
    Then failed with the message:
    """
    Should end with `'`
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

  Scenario: record string position
    When evaluate by:
    """
    null: 'hello'
    """
    Then got the following notation:
    """
    null: 'hello'
          ^
    """
