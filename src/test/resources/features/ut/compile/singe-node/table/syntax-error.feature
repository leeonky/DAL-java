Feature: syntax error

  Scenario: syntax error too many headers
    Given the following dal code:
    """
    | name  |
    | 'Tom' | 30 |
    """
    Then failed to get "table" node with the following message:
    """
    Different cell size
    """
    And got the following source code information:
    """
    | name  |
    | 'Tom' | 30 |
              ^
    """

  Scenario: syntax error too many cells
    Given the following dal code:
    """
    | name  | age |
    | 'Tom' |
    """
    Then failed to get "table" node with the following message:
    """
    Different cell size
    """
    And got the following source code information:
    """
    | name  | age |
    | 'Tom' |
             ^
    """

  Scenario: syntax error missing |
    Given the following dal code:
    """
    | name
    """
    Then failed to get "table" node with the following message:
    """
    Should end with `|`
    """
    And got the following source code information:
    """
    | name
          ^
    """

  Scenario: should raise error when invalid table
    When assert by the following code:
    """
    = | name   | age |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
      | 'Lily' | 20  |
    """
    Then failed with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
    = | name   | age |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
        ^
      | 'Lily' | 20  |
    """
    When assert by the following code:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
      | 'Tom'  | 10  |
      | ...          |
    """
    Then failed with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
        ^
      | 'Tom'  | 10  |
      | ...          |
    """
    When assert by the following code:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
      | 'Tom'  | 10  |
    """
    Then failed with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
    = | name   | age |
      | 'Lily' | 20  |
      | ...          |
        ^
      | 'Tom'  | 10  |
    """
