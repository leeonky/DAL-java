Feature: compile identity property

  Scenario Outline: return null when not valid identity
    Given the following dal code:
    """
    <code>
    """
    Then got the following "identity-property" node:
    """
    : null
    """
    Examples:
      | code   |
      | (      |
      | )      |
      | =      |
      | >      |
      | <      |
      | +      |
      | -      |
      | *      |
      | /      |
      | &      |
      | !      |
      | ,      |
      | [      |
      | ]      |
      | :      |
      | \|     |
      | \n     |
      | `TAB   |
      | `SPACE |
      | is     |
      | which  |
      | null   |
      | true   |
      | false  |
      | or     |
      | and    |
      | 100    |

  Scenario Outline: valid identity property
    Given the following dal code:
    """
      <code>
    """
    Then got the following "identity-property" node:
    """
    : {
      class.simpleName: 'SymbolNode'
      positionBegin: 2
      inspect: '<code>'
    }
    """
    Examples:
      | code |
      | a    |
      | 1a   |
      | _1a  |
      | 1_d  |

  Scenario Outline: identity property should end with delimiter and dot
    Given the following dal code:
    """
    name<delimiter>xxx
    """
    Then got the following "identity-property" node:
    """
    : {
      class.simpleName: 'SymbolNode'
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
      | .         |

  Scenario Outline: identity-property start with match key word
    Given the following dal code:
    """
    <identity-property>
    """
    Then got the following "identity-property" node:
    """
    : {
      class.simpleName: 'SymbolNode'
      inspect: '<identity-property>'
    }
    """
    Examples:
      | identity-property |
      | orderCount        |
      | isOk              |
