Feature: others

  Scenario: compare null
    * the following verification should pass:
    """
    null= null and null: null
    """
    And the following verification should failed:
    """
    0: null
    """
    And the following verification should failed:
    """
    false: null
    """
    And the following verification should failed:
    """
    "": null
    """
    And the following verification should failed:
    """
    null: false
    """

  Scenario: wildcard verification
    Then the following verification should pass:
    """
      1: * and 1= * and null: * and null= *
    """
