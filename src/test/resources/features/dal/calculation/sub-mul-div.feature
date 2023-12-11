Feature: sub mul div

  Scenario Outline: number sub/mul/div
    When evaluate by:
    """
    <expression>
    """
    Then the result should:
    """
    = <result>
    """
    Examples:
      | expression | result |
      | 5-2        | 3      |
      | 5*2        | 10     |
      | 8/4        | 2      |
      | 5-2.0      | 3.0    |
      | 5*2.0      | 10.0   |
      | 8/4.0      | 2.0    |

  Scenario Outline: sub/mul/div with input
    Given the following json:
    """
    8
    """
    When evaluate by:
    """
    <expression>
    """
    Then the result should:
    """
    = <result>
    """
    Examples:
      | expression | result |
      | -(2)       | 6      |
      | *2         | 16     |
      | /2         | 4      |

  Scenario: raise error when operands are not number
    When evaluate by:
    """
    '5'-2
    """
    Then failed with the message:
    """
    No operation `SUB` between 'java.lang.String' and 'java.lang.Integer'
    """
    And got the following notation:
    """
    '5'-2
       ^
    """
    When evaluate by:
    """
    5-'2'
    """
    Then failed with the message:
    """
    No operation `SUB` between 'java.lang.Integer' and 'java.lang.String'
    """
    And got the following notation:
    """
    5-'2'
     ^
    """
    When evaluate by:
    """
    '5'*2
    """
    Then failed with the message:
    """
    No operation `MUL` between 'java.lang.String' and 'java.lang.Integer'
    """
    And got the following notation:
    """
    '5'*2
       ^
    """
    When evaluate by:
    """
    5*'2'
    """
    Then failed with the message:
    """
    No operation `MUL` between 'java.lang.Integer' and 'java.lang.String'
    """
    And got the following notation:
    """
    5*'2'
     ^
    """
    When evaluate by:
    """
    '5'/2
    """
    Then failed with the message:
    """
    No operation `DIV` between 'java.lang.String' and 'java.lang.Integer'
    """
    And got the following notation:
    """
    '5'/2
       ^
    """
    When evaluate by:
    """
    5/'2'
    """
    Then failed with the message:
    """
    No operation `DIV` between 'java.lang.Integer' and 'java.lang.String'
    """
    And got the following notation:
    """
    5/'2'
     ^
    """
