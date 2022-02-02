Feature: symbol

  Scenario Outline: return null when not valid identity
    Given the following dal expression:
    """
    <code>
    """
    Then parse the following "symbol" node:
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
    Given the following dal expression:
    """
      <code>
    """
    Then parse the following "symbol" node:
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

  Scenario Outline: identity should end with delimiter and dot
    Given the following dal expression:
    """
    name<delimiter>xxx
    """
    Then parse the following "symbol" node:
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

  Scenario Outline: identity start with match key word
    Given the following dal expression:
    """
    <identity-property>
    """
    Then parse the following "symbol" node:
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

  Scenario: evaluate symbol under current scope
    Given the following json:
    """
    {
      "name": "Tom"
    }
    """
    When evaluate follow expression as "symbol" node:
    """
      name
    """
    Then the result should:
    """
    : 'Tom'
    """
