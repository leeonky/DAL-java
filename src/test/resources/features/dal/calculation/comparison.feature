Feature: comparison

  Scenario Outline: compare number
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code | value |
      | 2>1  | true  |
      | 1>2  | false |
      | 1>=0 | true  |
      | 1>=1 | true  |
      | 1>=2 | false |
      | 1<=2 | true  |
      | 1<=1 | true  |
      | 1<=0 | false |

  Scenario Outline: compare string
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code    | value |
      | 'a'<'b' | true  |
      | 'b'<'a' | false |
      | 'b'>'a' | true  |
      | 'a'>'b' | false |

  Scenario Outline: raise error when compare with null
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
      | op1  | opt | op2  | message                           |
      | null | >   | null | Can not compare [null] and [null] |
      | null | >   | 1    | Can not compare [null] and [1]    |
      | 1    | >   | null | Can not compare [1] and [null]    |

  Scenario: raise error when not supported
    When evaluate by:
    """
    '' > 1
    """
    Then failed with the message:
    """
    Can not compare [java.lang.String: ] and [java.lang.Integer: 1]
    """
    And got the following notation:
    """
    '' > 1
       ^
    """

  Scenario Outline: compare with input
    Given the following json:
    """
    2
    """
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code | value |
      | >1   | true  |
      | <1   | false |
