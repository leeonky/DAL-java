Feature: support line comment

  Scenario Outline: first line start with comment char
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment>
    = 1
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario Outline: multi comment lines
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment> comment 1
    <comment> comment 2
    = 1
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario Outline: blank line between comment lines
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment> comment 1

    <comment> comment 2
    = 1
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario Outline: only comments with no code
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment>
    """
    Examples:
      | comment |
      | #       |
      | //      |
