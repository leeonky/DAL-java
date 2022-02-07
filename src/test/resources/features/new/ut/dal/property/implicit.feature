Feature: symbol

  Scenario Outline: access input object
    Given the following json:
    """
      {
        "id": 100,
        "_name": "Tom",
        "1a": "start with number"
      }
    """
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    Examples:
      | code  | value               |
      | id    | 100                 |
      | _name | 'Tom'               |
      | 1a    | 'start with number' |


  Scenario Outline: identity start with match key word
    Given the following json:
    """
      {
        "<identity-property>": 100
      }
    """
    When evaluate by:
    """
    <identity-property>
    """
    Then the result should:
    """
    : 100
    """
    Examples:
      | identity-property |
      | orderCount        |
      | isOk              |

  Scenario: record position of implicit property
    Given evaluate by:
    """
      property
    """
    Then got the following notation:
    """
      property
      ^
    """

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
