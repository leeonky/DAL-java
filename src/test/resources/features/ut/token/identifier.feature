Feature: identifier token

  Scenario Outline: parse as key word
    Given the following dal code:
    """
    <code>
    """
    Then got the following "keyWord" token:
    """
    : {
      type: 'KEY_WORD'
      value: '<value>'
    }
    """
    Examples:
      | code  | value |
      | is    | is    |
      | which | which |

  Scenario Outline: parse as const value
    Given the following dal code:
    """
    <code>
    """
    Then got the following "keyWord" token:
    """
    : {
      type: 'CONST_VALUE'
      value: <value>
    }
    """
    Examples:
      | code  | value |
      | null  | null  |
      | true  | true  |
      | false | false |

  Scenario Outline: parse as operator
    Given the following dal code:
    """
    <code>
    """
    Then got the following "keyWord" token:
    """
    : {
      type: 'OPERATOR'
      value: '<value>'
    }
    """
    Examples:
      | code | value |
      | and  | and   |
      | or   | or    |

  Scenario Outline: parse as identifier
    Given the following dal code:
    """
    <code>
    """
    Then got the following "identifier" token:
    """
    : {
      type: 'IDENTIFIER'
      value: '<value>'
    }
    """
    Examples:
      | code          | value         |
      | hello         | hello         |
      | order.product | order.product |
      | _001          | _001          |
      | order_1       | order_1       |

  Scenario Outline: identifier delimiter
    Given the following dal code:
    """
    xxx<delimiter>
    """
    Then got the following "identifier" token:
    """
    : {
      type: 'IDENTIFIER'
      value: 'xxx'
    }
    """
    And current offset char of source code is "<delimiter>"
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
