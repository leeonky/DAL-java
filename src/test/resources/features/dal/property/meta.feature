Feature: access object meta property by ::xxx

  Scenario: access list size
    Given the following json:
    """
      [1, 2]
    """
    When evaluate by:
    """
    ::size
    """
    Then the result should:
    """
    : 2
    """
    Then the inspect should:
    """
    ::size
    """

  Scenario: raise error when target is not list
    Given the following json:
    """
      {
        "data": "not list"
      }
    """
    When evaluate by:
    """
    data::size
    """
    Then failed with the message:
    """
    Input value is not list, only List support `size` meta property
    """
    And got the following notation:
    """
    data::size
    ^
    """

  Scenario: raise error when un-register meta property
    Given the following json:
    """
      {
        "data": "not list"
      }
    """
    When evaluate by:
    """
    data::unRegister
    """
    Then failed with the message:
    """
    Meta property `unRegister` not found
    """
    And got the following notation:
    """
    data::unRegister
          ^
    """

#  TODO in table row / cell