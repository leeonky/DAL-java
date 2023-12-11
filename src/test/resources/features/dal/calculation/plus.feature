Feature: plus

  Scenario Outline: number + number
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code      | value |
      | 1 + 2     | 3     |
      | 2.0 + 3.0 | 5.0   |

  Scenario: string + string
    When evaluate by:
    """
    'a'+'b'
    """
    Then the result should:
    """
    = ab
    """

  Scenario: plus string and object
    When evaluate by:
    """
    'a'+ 1
    """
    Then the result should:
    """
    = a1
    """
    When evaluate by:
    """
    1 + 'a'
    """
    Then the result should:
    """
    = 1a
    """

  Scenario: with input
    Given the following json:
    """
    2
    """
    When evaluate by:
    """
    + 1
    """
    Then the result should:
    """
    = 3
    """

  Scenario Outline: raise error when unsupported number type
    Given the following json:
    """
    {
      "object": {},
      "num": 1
    }
    """
    When evaluate by:
    """
    <op1>
    <opt>
    <op2>
    """
    Then failed with the message:
    """
    <message>
    """
    And got the following notation:
    """
    <op1>
    <opt>
    ^
    <op2>
    """
    Examples:
      | op1    | opt | op2    | message                                                                       |
      | object | +   | num    | No operation `PLUS` between 'java.util.LinkedHashMap' and 'java.lang.Integer' |
      | num    | +   | object | No operation `PLUS` between 'java.lang.Integer' and 'java.util.LinkedHashMap' |
