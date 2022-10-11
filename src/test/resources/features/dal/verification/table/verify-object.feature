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
    row1 | 2   |
    """
    And the inspect should:
    """
    : | col |
    row0 | : 1 |
    row1 | : 2 |
    """

  Scenario: invalid property key table
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
    When evaluate by:
    """
    :    | col |
    row0 | 1   |
         | 2   |
    """
    Then failed with the message:
    """
    Row index should be consistent
    """
    And got the following notation:
    """
    :    | col |
    row0 | 1   |
    ^^^^^^^^^^^^^
         | 2   |
    ^^^^^^^^^^^^^
    """

  Scenario: verify all keys in object
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
    When evaluate by:
    """
     =    | col |
     row0 | 1   |
    """
    Then failed with the message:
    """
    Unexpected fields `row1`
    """
    And got the following notation:
    """
     =    | col |
     ^
     row0 | 1   |
    """

  Scenario: empty table for object
    Given the following json:
    """
    {}
    """
    Then the following verification should pass:
    """
     =    | col |
    """

  Scenario: transposed table one header and one row
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
    : | >>  | row0 |
      | col | 1    |
    """
    And the inspect should:
    """
    : | >> | row0 |
    | col | : 1 |
    """
