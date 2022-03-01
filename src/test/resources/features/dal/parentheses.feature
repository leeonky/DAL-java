Feature: parentheses node

  Scenario: single value in parentheses
    When evaluate by:
    """
     (1)
    """
    Then the result should:
    """
    : 1
    """
    And the inspect should:
    """
    (1)
    """
    When evaluate by:
    """
    1: (2)
    """
    Then got the following notation:
    """
    1: (2)
       ^
    """

  Scenario: expression in parentheses
    When evaluate by:
    """
     (1+1)
    """
    Then the result should:
    """
    : 2
    """
    And the inspect should:
    """
    (1 + 1)
    """
    When evaluate by:
    """
    1: (1+1)
    """
    Then got the following notation:
    """
    1: (1+1)
       ^
    """

  Scenario: raiser error when parentheses has no data
    When evaluate by:
    """
      ()
    """
    Then failed with the message:
    """
    Expect a value or expression
    """
    And got the following notation:
    """
      ()
       ^
    """

  Scenario: raiser error when parentheses is not finished
    When evaluate by:
    """
      (1
    """
    Then failed with the message:
    """
    Should end with `)`
    """
    And got the following notation:
    """
      (1
        ^
    """

  Scenario: raiser error when got unexpected token
    When evaluate by:
    """
      (1 1
    """
    Then failed with the message:
    """
    Should end with `)`
    """
    And got the following notation:
    """
      (1 1
         ^
    """
