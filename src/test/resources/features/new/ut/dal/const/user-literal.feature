Feature: user literal

  Scenario: define and not match user literal
    Given the following json:
    """
    {
      "$a": 1
    }
    """
    And defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    When evaluate by:
    """
    $a
    """
    Then the result should:
    """
    : {
      class.simpleName: 'Integer'
      toString: '1'
    }
    """
