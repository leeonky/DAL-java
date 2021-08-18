Feature: number token

  Scenario: return empty when first char is not digital
    Given the follow dal code:
    """
    not starts with digital
    """
    Then got the following "number" token:
    """
    : null
    """

  Scenario: return empty when invalid number, and source code offset should rollback to beginning
    Given the follow dal code:
    """
    12Invalid
    """
    Then got the following "number" token:
    """
    : null
    """
    Then got the following "identifier" token:
    """
    : {
      type: 'IDENTIFIER'
      value: '12Invalid'
    }
    """

  Scenario Outline: parse number to const value token in any number format
    Given the follow dal code:
    """
    <number>=1
    """
    Then got the following "number" token:
    """
    : {
      type: 'CONST_VALUE'
      value: <token>
    }
    """
    Examples:
      | number | token |
      | 100    | 100   |
      | 0x100  | 256   |
      | 0.1    | 0.1   |

  Scenario: number token can be finished by the end of source code
    Given the follow dal code:
    """
    100
    """
    Then got the following "number" token:
    """
    : {
      type: 'CONST_VALUE'
      value: 100
    }
    """

  Scenario Outline: delimiter after number token
    Given the follow dal code:
    """
    100<delimiter>
    """
    When got the following "number" token:
    """
    : {
      type: 'CONST_VALUE'
      value: 100
    }
    """
    Then current offset char of source code is "<delimiter>"
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
