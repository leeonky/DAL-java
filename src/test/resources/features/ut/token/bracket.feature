Feature: ()[]{}

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

  Scenario Outline: return corresponding token
    Given the follow dal code:
    """
    <brace>
    """
    Then got the following "<brace>" token:
    """
    : {
      type: '<type>'
      value: '<brace>'
    }
    """
    Examples:
      | brace | type                |
      | (     | OPENING_PARENTHESIS |
      | )     | CLOSING_PARENTHESIS |
      | [     | OPENING_BRACKET     |
      | ]     | CLOSING_BRACKET     |
      | {     | OPENING_BRACE       |
      | }     | CLOSING_BRACE       |

  Scenario Outline: seek to next char after fetch token
    Given the follow dal code:
    """
    <brace>=
    """
    When take an "<brace>" token
    Then current offset char of source code is "="
    Examples:
      | brace |
      | (     |
      | )     |
      | [     |
      | ]     |
      | {     |
      | }     |
