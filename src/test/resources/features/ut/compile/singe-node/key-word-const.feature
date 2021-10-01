Feature: compile key word const value (true false null)

  Scenario: compile const true
    Given the following dal code:
    """
     true
    """
    Then got the following "const-true" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'true'
      positionBegin: 1
    }
    """
    And evaluate result is:
    """
    : true
    """

  Scenario: compile const false
    Given the following dal code:
    """
     false
    """
    Then got the following "const-false" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'false'
      positionBegin: 1
    }
    """
    And evaluate result is:
    """
    : false
    """

  Scenario: compile const null
    Given the following dal code:
    """
     null
    """
    Then got the following "const-null" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'null'
      positionBegin: 1
    }
    """
    And evaluate result is:
    """
    : null
    """
