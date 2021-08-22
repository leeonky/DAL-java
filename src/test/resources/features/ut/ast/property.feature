Feature: compile to property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "property" node:
    """
    : null
    """
