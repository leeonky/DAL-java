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

  Scenario: multiple line text block
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
