Feature: calculation

  Scenario Outline: calculation
    When evaluate by the following code:
    """
    <code>
    """
    Then single result is:
    """
    = <value>
    """
    Examples:
      | code              | value |
      | 1 + 1             | 2     |
      | 2 * 3             | 6     |
      | 4 / 2             | 2     |
      | 5 - 4             | 1     |
      | (2+3)*(4+4)/(2+6) | 5     |
      | 2+3*4+4/2+1       | 17    |

  Scenario Outline: calculation with input
    Given the following input data:
    """
    <input>
    """
    When evaluate by the following code:
    """
    <code>
    """
    Then single result is:
    """
    = <value>
    """
    Examples:
      | input | code   | value |
      | 1     | +1     | 2     |
      | 5     | -4     | 1     |
      | 2     | * 3    | 6     |
      | 4     | / 2    | 2     |
      | 2     | +3*2   | 8     |
      | 2     | *(3+4) | 14    |

  Scenario Outline: logic operator
    When evaluate by the following code:
    """
    <code>
    """
    Then single result is:
    """
    = <value>
    """
    Examples:
      | code                 | value |
      | !false               | true  |
      | true and true        | true  |
      | true and false       | false |
      | false or true        | true  |
      | true , true          | true  |
      | true , false         | false |
      | true, true, true     | true  |
      | true, true, false    | false |
      | true, true and false | false |

  Scenario Outline: comparation
    When evaluate by the following code:
    """
    <code>
    """
    Then single result is:
    """
    = <value>
    """
    Examples:
      | code | value |
      | 2>1  | true  |
      | 2<1  | false |
      | 1>=1 | true  |
      | 1>=2 | false |
      | 1<=2 | true  |
      | 2<=1 | false |

  Scenario Outline: raise error when calculation with null
    When evaluate by:
    """
    null <operator> 1
    """
    Then failed with the message:
    """
    <message>
    """
    And got the following notation:
    """
    null <operator> 1
         ^
    """
    Examples:
      | operator | message                                     |
      | +        | Can not plus 'null' and 'java.lang.Integer' |
      | >        | Can not compare [null] and [1]              |
