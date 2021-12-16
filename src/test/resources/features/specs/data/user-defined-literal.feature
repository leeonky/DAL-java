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
      class.simpleName: 'USDollar'
    """

  Scenario: define and not match user literal
    Given defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    And the following dal code:
    """
    $a
    """
    And the following input data:
    """
    {
      "$a": 1
    }
    """
    Then single result is:
    """
    : {
      class.simpleName: 'Integer'
      toString: '1'
    }
    """
