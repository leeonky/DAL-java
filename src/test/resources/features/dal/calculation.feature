Feature: calculation

  Scenario Outline: calculation
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code              | value |
      | 1 + 1             | 2     |
      | '1' + '1'         | '11'  |
      | 1 + '1'           | '11'  |
      | '1' + 1           | '11'  |
      | 2 * 3             | 6     |
      | 4 / 2             | 2     |
      | 5 - 4             | 1     |
      | (2+3)*(4+4)/(2+6) | 5     |
      | 2+3*4+4/2+1       | 17    |

  Scenario Outline: calculation with input
    Given the following json:
    """
    <input>
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
      | input | code   | value |
      | 1     | +1     | 2     |
      | 5     | -(4)   | 1     |
      | 2     | * 3    | 6     |
      | 4     | / 2    | 2     |
      | 2     | +3*2   | 8     |
      | 2     | *(3+4) | 14    |

  Scenario Outline: logic operator
    When evaluate by:
    """
    <code>
    """
    Then the result should:
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

  Scenario Outline: raise error when unsupported number type
    Given the following json:
    """
    {
      "str": {},
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
      | op1 | opt | op2 | message                                                                         |
      | num | +   | str | Can not plus 'java.lang.Integer' and 'java.util.LinkedHashMap'                  |
      | num | -   | str | Operands should be number but 'java.lang.Integer' and 'java.util.LinkedHashMap' |
      | num | *   | str | Operands should be number but 'java.lang.Integer' and 'java.util.LinkedHashMap' |
      | num | /   | str | Operands should be number but 'java.lang.Integer' and 'java.util.LinkedHashMap' |
