Feature: basic data and type

  Scenario: return input value when no code
    Given the following input data:
    """
    true
    """
    And the following dal code:
    """
    """
    Then single result is:
    """
    = true
    """

  Scenario: const value
    Given the following dal code:
    """
    true
    """
    Then single result is:
    """
    = true
    """
