Feature: ```string```

  Rule: common

    Scenario: empty string
      Given the following json:
      """
      {
        "key": "a"
      }
      """
      When evaluate by:
      """
      key= ```
           ```
      """
      Then failed with the message:
      """
      Expected to be equal to: java.lang.String
      <>
       ^
      Actual: java.lang.String
      <a>
       ^
      """
      And got the following notation:
      """
      key= ```
           ```
           ^
      """

    Scenario: single line text block
      Given the following json:
      """
      {
        "key": "a"
      }
      """
      Then the following verification should pass:
      """
      key= ```
           a
           ```
      """

    Scenario: multiple line text block with indent
      Given the following json:
      """
      {
        "key": "a\nb"
      }
      """
      Then the following verification should pass:
      """
      key
         = ```
           a
           b
           ```
      """

    Scenario: raise error when missing end mark
      Given the following json:
      """
      {
        "key": "a"
      }
      """
      When evaluate by:
      """
      key= ```
           a
      """
      Then failed with the message:
      """
      Should end with '```'
      """
      And got the following notation:
      """
      key= ```
           a
            ^
      """
      When evaluate by:
      """
      key= ```
      """
      Then failed with the message:
      """
      Should end with '```'
      """
      And got the following notation:
      """
      key= ```
              ^
      """

  Rule: customer

    Scenario: customer number of notation `
      Given the following json:
      """
      {
        "key": "a\nb"
      }
      """
      Then the following verification should pass:
      """
      key= ````
           a
           b
           ````
      """
      And the inspect should:
      """
      key= ````
           a
           b
           ````
      """
      When evaluate by:
      """
      key= ````
      """
      Then failed with the message:
      """
      Should end with '````'
      """
      And got the following notation:
      """
      key= ````
               ^
      """

    Scenario: use LF to join lines
      Given the following json:
      """
      {
        "key": "a\nb"
      }
      """
      Then the following verification should pass:
      """
      key= ``` LF
           a
           b
           ```
      """
      And the inspect should:
      """
      key= ``` LF
           a
           b
           ```
      """

    Scenario: raise error when invalid block attribute
      Given the following json:
      """
      {
        "key": "a\nb"
      }
      """
      When evaluate by:
      """
      key= ``` CR not-exist
           a
           b
           ```
      """
      Then failed with the message:
      """
      Invalid text formatter `not-exist`, all supported formatters are:
        LF:
          use \n as new line
        CR:
          use \r as new line
        <:
          use < as end of line character
        \:
          use \ as line continuation character
        ⏎:
          use ⏎ as end of line character
      """
      And got the following notation:
      """
      key= ``` CR not-exist
                  ^
           a
           b
           ```
      """

    Scenario: use CR to join lines
      Given the following json:
      """
      {
        "key": "a\rb"
      }
      """
      Then the following verification should pass:
      """
      key= ``` CR
           a
           b
           ```
      """

    Scenario: override all attribute
      Given the following json:
      """
      {
        "key": "a\rb"
      }
      """
      Then the following verification should pass:
      """
      key= ``` LF CR
           a
           b
           ```
      """

    Scenario: customer tail char
      Given the following json:
      """
      {
        "key": "a \nb"
      }
      """
      Then the following verification should pass:
      """
      key= ```
           a <
           b<
           ```
      """

    Scenario: unicode tail char
      Given the following json:
      """
      {
        "key": "a \nb"
      }
      """
      Then the following verification should pass:
      """
      key= ``` ⏎
           a ⏎
           b
           ```
      """

    Scenario: mixin and override attribute
      Given the following json:
      """
      {
        "key": "a \rb"
      }
      """
      Then the following verification should pass:
      """
      key= ``` CR ⏎
           a ⏎
           b
           ```
      """

    Scenario: default continue char is \
      Given the following json:
      """
      {
        "key": "a b"
      }
      """
      Then the following verification should pass:
      """
      key= ```
           a \
           b
           ```
      """
