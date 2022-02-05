Feature: number

  Scenario Outline: number verification in same type
    Then the following verification should pass:
    """
      100<opt> 100
    """
    But the following verification should failed:
    """
      100<opt> 101
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: number eq should in same type, number match can be in different type
    Then the following verification should failed:
    """
      100= 100.0
    """
    But the following verification should pass:
    """
      100: 100.0
    """

  Scenario: not allow convert string to number implicitly
    * the following verification should failed:
    """
      '1': 1
    """
