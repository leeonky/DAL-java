Feature: syntax error

  Scenario: syntax error too many cells
    When evaluate by:
    """
    : | name  |
      | 'Tom' | 30 |
    """
    Then failed with the message:
    """
    Different cell size
    """
    And got the following notation:
    """
    : | name  |
      | 'Tom' | 30 |
                ^
    """

  Scenario: too many headers
    When evaluate by:
    """
    = | name  | age |
      | 'Tom' |
    """
    Then failed with the message:
    """
    Different cell size
    """
    And got the following notation:
    """
    = | name  | age |
      | 'Tom' |
        ^
    """

  Scenario: syntax error missing |
    When evaluate by:
    """
    = | name
    """
    Then failed with the message:
    """
    Should end with `|`
    """
    And got the following notation:
    """
    = | name
            ^
    """

  Scenario: should raise error when invalid table
    When evaluate by:
    """
    = | name   | age |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
      | 'Lily' | 20  |
    """
    Then failed with the message:
    """
    Invalid ellipsis
    """
    And got the following notation:
    """
    = | name   | age |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
        ^
      | 'Lily' | 20  |
    """
    When evaluate by:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
    """
    When evaluate by:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
      | 'Tom'  | 10  |
    """
    Then failed with the message:
    """
    Invalid ellipsis
    """
    And got the following notation:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
        ^
      | 'Tom'  | 10  |
    """
    When evaluate by:
    """
    = | name   |
    0 | 'Tom'  |
      | 'John' |
    """
    Then failed with the message:
    """
    Row index should be consistent
    """
    And got the following notation:
    """
    = | name   |
    0 | 'Tom'  |
    ^^^^^^^^^^^^^
      | 'John' |
    ^^^^^^^^^^^^^
    """
