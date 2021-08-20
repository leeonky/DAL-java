Feature: bracket token

  Scenario Outline: return null when char dose not match
    Given the follow dal code:
    """
    =
    """
    Then got the following "<brace>" token:
    """
    : null
    """
    Examples:
      | brace |
      | (     |
      | )     |
      | [     |
      | ]     |
      | {     |
      | }     |

  Scenario Outline: return corresponding token, end seek to next char after fetch token
    Given the follow dal code:
    """
    <brace>=
    """
    Then got the following "<brace>" token:
    """
    : {
      type: '<type>'
      value: '<brace>'
    }
    """
    And current offset char of source code is "="
    Examples:
      | brace | type                |
      | (     | OPENING_PARENTHESIS |
      | )     | CLOSING_PARENTHESIS |
      | [     | OPENING_BRACKET     |
      | ]     | CLOSING_BRACKET     |
      | {     | OPENING_BRACE       |
      | }     | CLOSING_BRACE       |
