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
      Actual: java.lang.String
      <a>
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
      key= ```
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
      Should end with ```
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
      Should end with ```
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
      key= 'a
      b'
      """
      When evaluate by:
      """
      key= ````
      """
      Then failed with the message:
      """
      Should end with ````
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

    Scenario: raise error when invalid block attribute
      Given the following json:
      """
      {
        "key": "a\nb"
      }
      """
      When evaluate by:
      """
      key= ``` not-exist
           a
           b
           ```
      """
      Then failed with the message:
      """
      Invalid text block attribute `not-exist`, all supported attributes are:
        LF: use \n as new line
      """
      And got the following notation:
      """
      key= ``` not-exist
               ^
           a
           b
           ```
      """
