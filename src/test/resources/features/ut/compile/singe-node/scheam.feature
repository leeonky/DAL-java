Feature: compile schema node

  Scenario Outline: raise error when no code
    Given the following dal code:
    """
    <code>
    """
    Then failed to get "schema" node with the following message:
    """
    schema expression is not finished
    """
    Examples:
      | code   |
      | \n     |
      | `TAB   |
      | `SPACE |

  Scenario Outline: raise error when not valid identity
    Given the following dal code:
    """
    <code>
    """
    Then failed to get "schema" node with the following message:
    """
    operand of `is` must be schema type
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
    Given the following dal code:
    """
      <code>
    """
    Then got the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaNode'
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
    Given the following dal code:
    """
    name<delimiter>xxx
    """
    Then got the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaNode'
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
      | /         |
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
