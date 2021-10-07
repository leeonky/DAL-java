Feature: compile dot or bracket notation property

  Scenario: return null when not start with dot or bracket
    Given the following dal code:
    """
    not start with . or [
    """
    Then got the following "explicit-property" node:
    """
    : null
    """

  Scenario Outline: dot and bracket notation property
    Given the following dal code:
    """
    <code>
    """
    Then got the following "explicit-property" node:
    """
    class.simpleName: 'PropertyNode'
    """
    When the following input data:
    """
      {"a": 1}
    """
    Then node evaluate result is:
    """
    : 1
    """
    Examples:
      | code  |
      | .a    |
      | ['a'] |