Feature: compile property starts with dot

  Scenario: return null when code is ...
    Given the following dal code:
    """
    ...
    """
    Then got the following "dot-property" node:
    """
    : null
    """
