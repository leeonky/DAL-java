Feature: syntax-error

  Scenario: raise error when right operand is not schema
    When evaluate by:
    """
    is +
    """
    Then failed with the message:
    """
    expect a schema
    """
    And got the following notation:
    """
    is +
       ^
    """

  Scenario: raise error when missing schema at the end of code
    When evaluate by:
    """
    is
    """
    Then failed with the message:
    """
    expect a schema
    """
    And got the following notation:
    """
    is
      ^
    """

  Scenario: raise error when schema list not finished
    When evaluate by:
    """
    is Integer /
    """
    Then failed with the message:
    """
    expect a schema
    """
    And got the following notation:
    """
    is Integer /
                ^
    """

  Scenario: raise error when no closing ]
    When evaluate by:
    """
      is [Integer
    """
    Then failed with the message:
    """
    should end with `]`
    """
    And got the following notation:
    """
      is [Integer
                 ^
    """

  Scenario: not support element schema expression in multidimensional list
    When evaluate by:
    """
      is [[Integer]]
    """
    Then failed with the message:
    """
    Not support multidimensional schema
    """
    And got the following notation:
    """
      is [[Integer]]
           ^
    """

  Scenario: available schema syntax
    Given the following schema class:
    """
    public class Id {
      public static class Zero {
          public int id = 0;
      }
    }
    """
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is Id.Zero
    """
