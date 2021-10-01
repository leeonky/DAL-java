Feature: compile property node

  Scenario: return null when not valid
    Given the following dal code:
    """
    is
    """
    Then got the following "property" node:
    """
    : null
    """

  Scenario Outline: identity, dot or bracket notation property
    Given the following dal code:
    """
    <code>
    """
    Then got the following "property" node:
    """
    class.simpleName: 'PropertyNode'
    """
    When the following input data:
    """
      {"a": 1}
    """
    Then evaluate result is:
    """
    : 1
    """
    Examples:
      | code  |
      | a     |
      | .a    |
      | ['a'] |