Feature: bugs

  Scenario Outline: missing when start with keyword (true false null)
    When evaluate by:
    """
    true: <word>
    """
    Then failed with the message:
    """
    Cannot compare between java.lang.Boolean
    <true>
    and java.lang.String
    <<word>>
    """
    Examples:
      | word        |
      | trueBlabla  |
      | falseBlabla |
      | nullBlabla  |

  Scenario Outline: missing when start with keyword (and or)
    Given the following json:
    """
    {
      "key1": 1,
      "<key>": 2
    }
    """
    Then the following verification should pass:
    """
    : {
      key1: 1
      <key>: 2
    }
    """
    Examples:
      | key   |
      | order |
      | andXx |
