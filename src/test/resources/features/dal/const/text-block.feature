Feature: ```string```

  Rule: common use in assertion

    Scenario: resolve when no indent
      When evaluate by:
      """
      ```
      hello
      ```
      """
      Then the result should:
      """
      = hello
      """

    Scenario: empty string
      Given the following json:
      """
      {
        "key": "a",
        "empty": ""
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
      And the following verification should pass:
      """
      empty= ```
             ```
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

    Scenario: single new line text block
      Given the following json:
      """
      {
        "key": "\n"
      }
      """
      Then the following verification should pass:
      """
      key= ```

           ```
      """

    Scenario: two lines text block with indent
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

    Scenario: first line is new line in two lines
      Given the following json:
      """
      {
        "key": "\nb"
      }
      """
      Then the following verification should pass:
      """
      key
         = ```

           b
           ```
      """

    Scenario: last line is new line in two lines
      Given the following json:
      """
      {
        "key": "b\n"
      }
      """
      Then the following verification should pass:
      """
      key
         = ```
           b

           ```
      """

    Scenario: resolve indent by shortest indent
      Given the following json:
      """
      {
        "key": " a\nb"
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

    Scenario: multiple lines text block
      Given the following json:
      """
      {
        "key": "\na\nb\nc\n"
      }
      """
      Then the following verification should pass:
      """
      key
         = ```

           a
           b
           c

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
              Accept: java.lang.String
              Return: java.lang.String
        CR:
          use \r as new line
              Accept: java.lang.String
              Return: java.lang.String
        CRLF:
          use \r\n as new line
              Accept: java.lang.String
              Return: java.lang.String
        <:
          use < as end of line character
              Accept: java.lang.String
              Return: java.lang.String
        \:
          use \ as line continuation character
              Accept: java.lang.String
              Return: java.lang.String
        ⏎:
          use ⏎ as end of line character
              Accept: java.lang.String
              Return: java.lang.String
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

    Scenario: use CRLF to join lines
      Given the following json:
      """
      {
        "key": "a\r\nb"
      }
      """
      Then the following verification should pass:
      """
      key= ``` CRLF
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

  Rule: map text block

    Scenario: invalid type in object map chain in text block
      Given the following java class:
      """
      public class Bean {}
      """
      Given the following text formatter "NeedBean":
      """
      public class NeedBean extends TextFormatter<Bean, String> {
      }
      """
      When use a instance of java class "Bean" to evaluate:
      """
      ```  NeedBean
      any string
      ```
      """
      Then failed with the message:
      """
      Invalid text formatter, expect a formatter which accept java.lang.String but #package#Bean
      """
      And got the following notation:
      """
      ```  NeedBean
           ^
      any string
      ```
      """
