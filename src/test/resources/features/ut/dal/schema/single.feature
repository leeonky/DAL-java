Feature: single schema

  Scenario Outline: raise error when no code
    Given the following dal expression:
    """
    <code>
    """
    Then failed to parse "schema" with the following message:
    """
    expect a schema
    """
    Examples:
      | code   |
      | \n     |
      | `TAB   |
      | `SPACE |

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

  Scenario Outline: valid schema
    Given the following dal expression:
    """
      <code>
    """
    Then parse the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaComposeNode'
      positionBegin: 2
      inspect: '<code>'
    }
    """
    Examples:
      | code |
      | a    |
      | 1a   |
      | a.b  |
      | _1a  |

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
