Feature: boolean

  Scenario Outline: boolean verification
    Then the following verification should pass:
    """
      true <opt> true and false <opt> false
    """
    But the following verification should failed:
    """
      true <opt> false
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: not allow convert string to boolean implicitly
    * the following verification should failed:
    """
      'true': true
    """
