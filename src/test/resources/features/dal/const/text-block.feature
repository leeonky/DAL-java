Feature: ```string```

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

  Scenario: customer text block notation
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
