Feature: string-dump

  Rule: dump

    Scenario: should dump string in escape char
      Given the following json:
      """
      "\r\n\t\b\\"
      """
      Then evaluate by:
      """
      = xx
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.String
      <xx>
       ^
      Actual: java.lang.String
      <\r\n\t\b\\>
       ^
      """
