Feature: compile table node

  Scenario: support table with only header and header judgement operator was specified
    Given the following dal code:
    """
    | name: |
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: '| name: |'
    }
    """

  Scenario: support table with header and cell
    Given the following dal code:
    """
    | name: |
    | 'Tom' |
    """
    Then got the following "judgement-expression-operand" node:
    """
    : {
      class.simpleName: 'TableNode'
      inspect: "| name: |
    | 'Tom' |"
    }
    """

#TODO default table judgement operator to header
#TODO default header judgement operator to cell
#TODO specify cell judgement operator
