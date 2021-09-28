Feature: compile property node

  Scenario: return null when not valid
    Given the following dal code xx:
    """
    is
    """
    Then got the following "property" node xx:
    """
    : null
    """

  Scenario Outline: identity, dot or bracket notation property
    Given the following dal code xx:
    """
    <code>
    """
    Then got the following "property" node xx:
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
      | a     |
      | .a    |
      | ['a'] |