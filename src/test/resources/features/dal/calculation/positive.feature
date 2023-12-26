Feature: positive

  Scenario: not support number
    When evaluate by:
    """
    (+1)
    """
    Then failed with the message:
    """
    Operand should be list but 'java.lang.Integer'
    """
    And got the following notation:
    """
    (+1)
      ^
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
    (+list)
    """
    Then the result should:
    """
    = [1 2 3]
    """

  Scenario: raise error when operand is not list
    When evaluate by:
    """
    (+'a')
    """
    Then failed with the message:
    """
    Operand should be list but 'java.lang.String'
    """
