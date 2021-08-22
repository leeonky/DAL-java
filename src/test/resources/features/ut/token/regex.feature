Feature: regex token

  Scenario Outline: return empty when first char is not /
    Given the following dal code after operator "<operator>":
    """
    {}
    """
    Then got the following "regex" token:
    """
    : null
    """
    Examples:
      | operator |
      | :        |
      | =        |

  Scenario Outline: regex must after judgement operator
    Given the following dal code after operator "<operator>":
    """
    /hello/
    """
    And take an "operator" token
    Then got the following "regex" token:
    """
    : null
    """
    Examples:
      | operator |
      | +        |
      | -        |

  Scenario Outline: regex must after judgement operator(= :)
    Given the following dal code after operator "<operator>":
    """
    /hello/
    """
    Then got the following "regex" token:
    """
    : {
      type: 'REGEX'
      value: 'hello'
    }
    """
    Examples:
      | operator |
      | :        |
      | =        |

  Scenario Outline: 2 escape chars (\\ => \, \/ => /)
    Given the following dal code after operator "=":
    """
    <code>
    """
    Then got the following "regex" token:
    """
    : {
      type: 'REGEX'
      value: '<regex>'
    }
    """
    Examples:
      | code     | regex  |
      | /\\\\/   | \\\\   |
      | /\\//    | /      |
      | /\\keep/ | \\keep |

  Scenario: seek to next char after fetch token
    Given the following dal code after operator "=":
    """
    /regex/&
    """
    When take an "regex" token
    Then current offset char of source code is "&"

  Scenario: syntax error: incomplete regex
    Given the following dal code after operator "=":
    """
    /regex
    """
    Then failed to take "regex" token with the following message:
    """
    string should end with `/`
    """
    And got the following source code information:
    """
    =/regex
           ^
    """

  Scenario: syntax error: incomplete escape char regex
    Given the following dal code after operator "=":
    """
    /regex\
    """
    Then failed to take "regex" token with the following message:
    """
    string should end with `/`
    """
    And got the following source code information:
    """
    =/regex\
            ^
    """
