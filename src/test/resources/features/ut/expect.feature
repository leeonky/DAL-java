Feature: expect - assertion

  Scenario: pass with dal code
    Given the following json:
    """
      { "key": 100 }
    """
    Then the following expectation should pass:
    """
    = {
      key= 100
    }
    """

  Scenario: failed with assert error with dal code
    Given the following json:
    """
      { "key": 100 }
    """
    When expect by the following code:
    """
    = {
      key= 200
    }
    """
    Then assert error with the message:
    """

    = {
      key= 200
           ^
    }

    Expected to be equal to: java.lang.Integer
    <200>
     ^
    Actual: java.lang.Integer
    <100>
     ^

    The root value was: {
        key: java.lang.Integer <100>
    }
    """

  Scenario: exact pass
    Given the following json:
    """
      { "key": 100 }
    """
    Then the following exact expectation should pass:
    """
    {
      key= 100
    }
    """

  Scenario: failed with assert error with exact verification
    Given the following json:
    """
      { "key": 100 }
    """
    When expect exact by the following code:
    """
    {
      key= 200
    }
    """
    Then assert error with the message:
    """

    {
      key= 200
           ^
    }

    Expected to be equal to: java.lang.Integer
    <200>
     ^
    Actual: java.lang.Integer
    <100>
     ^

    The root value was: {
        key: java.lang.Integer <100>
    }
    """

  Scenario: matching pass
    Given the following json:
    """
      { "key": 100 }
    """
    Then the following matching expectation should pass:
    """
    {
      key= 100
    }
    """

  Scenario: failed with assert error with matching verification
    Given the following json:
    """
      { "key": 100 }
    """
    When expect matching by the following code:
    """
    {
      key= 200
    }
    """
    Then assert error with the message:
    """

    {
      key= 200
           ^
    }

    Expected to be equal to: java.lang.Integer
    <200>
     ^
    Actual: java.lang.Integer
    <100>
     ^

    The root value was: {
        key: java.lang.Integer <100>
    }
    """
