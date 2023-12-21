Feature: converter

  Scenario: failed to convert
    Given the following json:
    """
    {
      "object": {}
    }
    """
    When evaluate by:
    """
    "invalid type": .object
    """
    Then failed with the message:
    """
    Cannot convert from java.lang.String to class java.util.LinkedHashMap
    """
    And got the following notation:
    """
    "invalid type": .object
    ^
    """
