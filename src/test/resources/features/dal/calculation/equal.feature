Feature: equal

  Scenario: compare null
    Then the following verification should pass:
    """
    null= null
    """
    When evaluate by:
    """
    1= null
    """
    Then failed with the message:
    """
    Expected to be equal to: null
                             ^
    Actual: java.lang.Integer
            ^
    <1>
    """
    And got the following notation:
    """
    1= null
       ^
    """

  Scenario: compare list
    Given the following json:
    """
    {
      "list": [],
      "empty": [],
      "list1": [1]
    }
    """
    Then the following verification should pass:
    """
    list= .empty
    """
    When evaluate by:
    """
    list= .list1
    """
    Then failed with the message:
    """
    Expected to be equal to: [
                              ^
        java.lang.Integer <1>
    ]
    Actual: []
             ^
    """
    And got the following notation:
    """
    list= .list1
          ^
    """

  Scenario: compare other object
    Then the following verification should pass:
    """
    1= 1
    """
    When evaluate by:
    """
    1= 2
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.Integer
    <2>
     ^
    Actual: java.lang.Integer
    <1>
     ^
    """
    And got the following notation:
    """
    1= 2
       ^
    """

#  customized null, customized list