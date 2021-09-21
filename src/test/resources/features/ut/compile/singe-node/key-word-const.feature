Feature: compile key word const value (true false null)

  Scenario: compile const true
    Given the following dal code xx:
    """
     true
    """
    Then got the following "const-true" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'true'
      positionBegin: 1
    }
    """
    And evaluate result is xx:
    """
    : true
    """

  Scenario: compile const false
    Given the following dal code xx:
    """
     false
    """
    Then got the following "const-false" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'false'
      positionBegin: 1
    }
    """
    And evaluate result is xx:
    """
    : false
    """

  Scenario: compile const null
    Given the following dal code xx:
    """
     null
    """
    Then got the following "const-null" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'null'
      positionBegin: 1
    }
    """
    And evaluate result is xx:
    """
    : null
    """
