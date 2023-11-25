Feature: negative

  Scenario: negate number for number

    When evaluate by:
    """
    -1
    """
    Then the result should:
    """
    = 1-2
    """
    When evaluate by:
    """
    -1.0
    """
    Then the result should:
    """
    = 1.0-2.0
    """

  Scenario: sort list for list
    Given the following json:
    """
    {
      "list": [1,3,2]
    }
    """
    When evaluate by:
    """
    (-list)
    """
    Then the result should:
    """
    = [3 2 1]
    """

  Scenario: raise error when operand is not number or list
    When evaluate by:
    """
    (-'a')
    """
    Then failed with the message:
    """
    Operand should be number or list but 'java.lang.String'
    """
