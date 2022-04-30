Feature: verify object

  Scenario: table one header and one row
    Given the following json:
    """
    {
      "row0": {
        "col": 1
      },
      "row1": {
        "col": 2
      }
    }
    """
    Then the following verification should pass:
    """
    :    | col |
    row0 | 1   |
    """
    And the inspect should:
    """
    : | col |
    row0 | col: 1 |
    """
