Feature: bugs

  Scenario Outline: incorrect key word
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
