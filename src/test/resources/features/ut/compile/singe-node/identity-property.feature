Feature: compile identity property

  Scenario Outline: return null when not valid identity
    Given the following dal code xx:
    """
    <code>
    """
    Then got the following "identity-property" node xx:
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

  Scenario Outline: valid identity property(may not possible)
    Given the following dal code xx:
    """
      <code>
    """
    Then got the following "identity-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '<code>'
    }
    """
    Examples:
      | code |
      | a    |
      | 1a   |

  Scenario Outline: identity property should end with delimiter and dot
    Given the following dal code xx:
    """
    name<delimiter>xxx
    """
    Then got the following "identity-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
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
