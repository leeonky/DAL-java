Feature: object-scope-relax-property

  Scenario Outline: support string property
    Given the following json:
    """
    {
      "user": {
        "name": "Tom"
      }
    }
    """
    Then the following verification should pass:
    """
    : {
      'user'<opt>'name': Tom
    }
    """
    And the inspect should:
    """
    : {'user'<opt>'name': 'Tom'}
    """
    Examples:
      | opt |
      | .   |
      | /   |

  Scenario: supported chars in relax property
    Given the following json:
    """
    {
      "-+%;": {
        "-+%;": "Tom"
      }
    }
    """
    Then the following verification should pass:
    """
    : {
      -+%;.-+%;: Tom
    }
    """
    And the inspect should:
    """
    : {-+%;.-+%;: 'Tom'}
    """
