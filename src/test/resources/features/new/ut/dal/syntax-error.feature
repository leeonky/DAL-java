Feature: syntax error expression

  Scenario Outline: missing operand2 with input data
    When evaluate by:
    """
    <opt>
    """
    Then failed with the message:
    """
    <message>
    """
    Examples:
      | opt  | message                      |
      | +    | expect a value or expression |
      | -    | expect a value or expression |
      | *    | expect a value or expression |
      | /    | expect a value or expression |
      | &&   | expect a value or expression |
      | \|\| | expect a value or expression |
      | and  | expect a value or expression |
      | or   | expect a value or expression |
      | ,    | expect a value or expression |
      | >    | expect a value or expression |
      | <    | expect a value or expression |
      | >=   | expect a value or expression |
      | <=   | expect a value or expression |
      | !=   | expect a value or expression |
      | =    | expect a value or expression |
      | :    | expect a value or expression |

  Scenario Outline: missing operand2
    When evaluate by:
    """
    1 <opt>
    """
    Then failed with the message:
    """
    <message>
    """
    Examples:
      | opt  | message                      |
      | +    | expect a value or expression |
      | -    | expect a value or expression |
      | *    | expect a value or expression |
      | /    | expect a value or expression |
      | &&   | expect a value or expression |
      | \|\| | expect a value or expression |
      | and  | expect a value or expression |
      | or   | expect a value or expression |
      | ,    | expect a value or expression |
      | >    | expect a value or expression |
      | <    | expect a value or expression |
      | >=   | expect a value or expression |
      | <=   | expect a value or expression |
      | !=   | expect a value or expression |
      | =    | expect a value or expression |
      | :    | expect a value or expression |

  Scenario: unexpected token after schema
    When evaluate by:
    """
    1 is Number .toString
    """
    Then failed with the message:
    """
    more than one expression
    """
    And got the following notation:
    """
    1 is Number .toString
                ^
    """
    When evaluate by:
    """
    1 is Number + 1
    """
    Then failed with the message:
    """
    expect a value or expression
    """
    And got the following notation:
    """
    1 is Number + 1
                ^
    """
