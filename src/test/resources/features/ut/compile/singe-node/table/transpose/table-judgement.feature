Feature: compile table with judgement

  Scenario: table with only headers
    Given the following dal code:
    """
    >>| name |
      | age  |
    """
    Then got the following "table" node:
    """
    inspect: '>>| name |
    | age |'
    """

#  Scenario: compile table with only default table judgement operator
#    Given the following dal code:
#    """
#    = >>| name | 'Tom'  |
#    """
#    Then got the following "expression" node:
#    """
#    rightOperand: {
#      class.simpleName: 'TableNode'
#      inspect: ">>| name | = 'Tom' |"
#    }
#    """

  Scenario: judgement table with empty list
    Given the following input data:
    """
    [ 1 ]
    """
    When assert by the following code:
    """
    = >>| name |
    """
    Then failed with the following message:
    """
    Expecting list size to be <0> but was <1>
    """
    And got the following source code information:
    """
    = >>| name |
    ^
    """
    When the following input data:
    """
    []
    """
    Then the following assertion should pass:
    """
    = >>| name |
    """

#TODO table transpose
