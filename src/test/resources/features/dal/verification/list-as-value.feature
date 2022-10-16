Feature: list as value

  Scenario: compare list as one value
    Given the following json:
    """
    {
      "str": "a"
    }
    """
    Then the following verification should pass:
    """
    str.bytes= 'a'.bytes
    """
    And the inspect should:
    """
    str.bytes= 'a'.bytes
    """
    Then the following verification should pass:
    """
    str.bytes: 'a'.bytes
    """
    And the inspect should:
    """
    str.bytes: 'a'.bytes
    """
    When evaluate by:
    """
    str.bytes: 'b'.bytes
    """
    Then failed with the message:
    """
    Expected to match: [
        java.lang.Byte <98>
                         ^
    ]
    Actual: [
        java.lang.Byte <97>
                         ^
    ]
    """
    And got the following notation:
    """
    str.bytes: 'b'.bytes
               ^
    """
