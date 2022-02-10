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
