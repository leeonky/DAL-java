Feature: syntax-error

  Scenario Outline: raise error when right operand is not schema
    When evaluate by:
    """
    is <schema>
    """
    Then failed with the message:
    """
    expect a schema
    """
    And got the following notation:
    """
    is <schema>
       ^
    """
    Examples:
      | schema   |
      | +        |
      | (Schema) |

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
    expect a schema
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

  Scenario Outline: raise error when not valid identity
    Given the following dal expression:
    """
    <code>
    """
    Then failed to parse "schema" with the following message:
    """
    expect a schema
    """
    Examples:
      | code  |
      | (     |
      | )     |
      | =     |
      | >     |
      | <     |
      | +     |
      | -     |
      | *     |
      | /     |
      | &     |
      | !     |
      | ,     |
      | [     |
      | ]     |
      | :     |
      | \|    |
      | is    |
      | which |
      | null  |
      | true  |
      | false |
      | or    |
      | and   |
      | 100   |

  Scenario Outline: schema should end with delimiter
    Given the following dal expression:
    """
    name<delimiter>xxx
    """
    Then parse the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaComposeNode'
      inspect: 'name'
    }
    """
    Examples:
      | delimiter |
      | (         |
      | )         |
      | =         |
      | >         |
      | <         |
      | +         |
      | -         |
      | *         |
      | &         |
      | !         |
      | ,         |
      | [         |
      | ]         |
      | :         |
      | \|        |
      | \n        |
      | `TAB      |
      | `SPACE    |
