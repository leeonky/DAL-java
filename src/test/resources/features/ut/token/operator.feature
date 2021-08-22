Feature: operator token

  Scenario: return null when char dose not match
    Given the following dal code:
    """
    not start with operator char
    """
    Then got the following "operator" token:
    """
    : null
    """

  Scenario Outline: not parse as / after judgement operator
    Given the following dal code after operator "<judgement>":
    """
    /
    """
    Then got the following "operator" token:
    """
    : null
    """
    Examples:
      | judgement |
      | =         |
      | :         |

  Scenario Outline: all supported operators, end seek to next char after fetching
    Given the following dal code:
    """
    <operator>1
    """
    Then got the following "operator" token:
    """
    : {
      type: 'OPERATOR'
      value: '<operator>'
    }
    """
    And current offset char of source code is "1"
    Examples:
      | operator |
      | -        |
      | !        |
      | =        |
      | >        |
      | <        |
      | +        |
      | *        |
      | /        |
      | :        |
      | >=       |
      | <=       |
      | !=       |
      | &&       |
      | \|\|     |
      | ,        |

  Scenario Outline: all supported operators at the end of code
    Given the following dal code:
    """
    <operator>
    """
    Then got the following "operator" token:
    """
    : {
      type: 'OPERATOR'
      value: '<operator>'
    }
    """
    Examples:
      | operator |
      | -        |
      | !        |
      | =        |
      | >        |
      | <        |
      | +        |
      | *        |
      | /        |
      | :        |
      | >=       |
      | <=       |
      | !=       |
      | &&       |
      | \|\|     |
      | ,        |

  Scenario Outline: distinguish regex after operator judgement
    Given the following dal code:
    """
    <judgement>/
    """
    Then got the following "operator" token:
    """
    : {
      type: 'OPERATOR'
      value: '<judgement>'
    }
    """
    Examples:
      | judgement |
      | :         |
      | =         |
