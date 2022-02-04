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
    class.simpleName: 'SymbolNode'
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
      | code |
      | a    |
#      TODO
#      | .a    |
#      | ['a'] |