Feature: relax header property

  Scenario Outline: support string property
    Given the following json:
    """
    [{
      "user": {
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | 'user'<opt>'name' |
      | Tom       |
    """
    And the inspect should:
    """
    : | 'user'<opt>'name' |
    | : 'Tom' |
    """
    Examples:
      | opt |
      | .   |
      | /   |

  Scenario Outline: supported chars in relax property
    Given the following json:
    """
    [{
      "%-+;": {
        "%-+;": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : | %-+;<opt>%-+; |
      | Tom       |
    """
    And the inspect should:
    """
    : | %-+;<opt>%-+; |
    | : 'Tom' |
    """
    Examples:
      | opt |
      | .   |
      | /   |
