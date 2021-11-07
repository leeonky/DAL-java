Feature: syntax error

  Scenario: incomplete header
    Given the following dal code:
    """
    >>| name
    """
    Then failed to get "table" node with the following message:
    """
    should end with `|`
    """
    And got the following source code information:
    """
    >>| name
            ^
    """
