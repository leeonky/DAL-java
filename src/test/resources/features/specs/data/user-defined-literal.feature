Feature: user defined literal

  Scenario: define and use user defined literal
    Given defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    And the following dal code:
    """
    $1
    """
    Then single result is:
    """
      class.simpleName: 'Money'
    """
