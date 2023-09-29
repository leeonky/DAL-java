Feature: syntax-error

  Scenario: raise error when more than one token in brackets
    Given evaluate by:
    """
    [1 2]
    """
    Then failed with the message:
    """
    Should end with `]`
    """
    And got the following notation:
    """
    [1 2]
       ^
    """

  Scenario: raise error when missed closing ']'
    Given evaluate by:
    """
    [100
    """
    Then failed with the message:
    """
    Should end with `]`
    """
    And got the following notation:
    """
    [100
        ^
    """

  Scenario: raise error when missed token and closing ']'
    Given evaluate by:
    """
    [
    """
    Then failed with the message:
    """
    Should end with `]`
    """
    And got the following notation:
    """
    [
     ^
    """
