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

  Scenario: incorrect value in const remark
    When evaluate by:
    """
    2 (1+2)
    """
    Then failed with the message:
    """
    Incorrect const remark, const value was java.lang.Integer
    <2>
    but remark (1 + 2) was java.lang.Integer
    <3>
    """
    And got the following notation:
    """
    2 (1+2)
      ^
    """

  Scenario: const remark for negative const value node
    Given the following json:
    """
    -2
    """
    Then the following verification should pass:
    """
    = -2 (1-3)
    """
