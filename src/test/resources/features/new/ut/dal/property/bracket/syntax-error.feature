Feature: syntax-error

  Scenario Outline: raise error when got invalid value in bracket
    Given the following dal expression:
    """
    [<code>]
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following notation:
    """
    [<code>]
     ^
    """
    Examples:
      | code |
      | +    |
      |      |

  Scenario: raise error when more than one token in brackets
    Given the following dal expression:
    """
    [1 2]
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following notation:
    """
    [1 2]
        ^
    """

  Scenario: raise error when missed closing ']'
    Given the following dal expression:
    """
    [100
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should end with `]`
    """
    And got the following notation:
    """
    [100
        ^
    """

  Scenario: raise error when missed token and closing ']'
    Given the following dal expression:
    """
    [
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should end with `]`
    """
    And got the following notation:
    """
    [
     ^
    """
