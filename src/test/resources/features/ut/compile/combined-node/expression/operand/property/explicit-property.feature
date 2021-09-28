Feature: compile dot or bracket notation property

  Scenario: return null when not start with dot or bracket
    Given the following dal code xx:
    """
    not start with . or [
    """
    Then got the following "explicit-property" node xx:
    """
    : null
    """

  Scenario Outline: dot and bracket notation property
    Given the following dal code xx:
    """
    <code>
    """
    Then got the following "explicit-property" node xx:
    """
    class.simpleName: 'PropertyNode'
    """
    When the following input data xx:
    """
      {"a": 1}
    """
    Then evaluate result is xx:
    """
    : 1
    """
    Examples:
      | code  |
      | .a    |
      | ['a'] |