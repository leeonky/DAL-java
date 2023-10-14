Feature: expect - run

  Scenario: expect a code block
    Given the following input code:
    """
    return 100;
    """
    Then the following verification of input code should pass:
    """
    = 100
    """

  Scenario: dump root value when input code
    Given the following input code:
    """
    return 100;
    """
    When expect run by the following code:
    """
    = 200
    """
    Then assert error with the message:
    """

    = 200
      ^

    Expected to be equal to: java.lang.Integer
    <200>
     ^
    Actual: java.lang.Integer
    <100>
     ^

    The root value was: InputCode return value was java.lang.Integer
    <100>
    """

  Scenario: dump exception when input code got error
    Given the following input code:
    """
    throw new java.lang.RuntimeException("Error");
    """
    When expect run by the following code:
    """
    = 200
    """
    Then assert error with the message:
    """

    Error

    Input code got exception: java.lang.RuntimeException {
    """
