Feature: commons

  Scenario: raise error when un-register meta property
    Given the following json:
    """
      {
        "data": "any object"
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

  Scenario: raise error when not list in list mapping
    Given the following json:
    """
      {
        "data": "not list"
      }
    """
    When evaluate by:
    """
    data::size[]
    """
    Then failed with the message:
    """
    Invalid input value, expect a List but: java.lang.String
    <not list>
    """
    And got the following notation:
    """
    data::size[]
              ^
    """
