Feature: const remark

  Scenario: const remark for const value node
    When evaluate by:
    """
    2 (1+1)
    """
    Then the result should:
    """
    : 2
    """
    And the inspect should:
    """
    2 (1 + 1)
    """

#    -2 ()