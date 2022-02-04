Feature: parentheses node

  Scenario: return null when does not match
    Given the following dal code:
    """
    not start with (
    """
    Then got the following "parentheses" node:
    """
    : null
    """

  Scenario: single value in parentheses
    Given the following dal code:
    """
     (1)
    """
    Then got the following "parentheses" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: '(1)'
      positionBegin: 1
    }
    """
    And node evaluate result is:
    """
    : 1
    """

  Scenario: expression in parentheses
    Given the following dal code:
    """
     (1+1)
    """
    Then got the following "parentheses" node:
    """
    : {
      class.simpleName: 'DALExpression'
      inspect: '(1 + 1)'
      positionBegin: 1
    }
    """
    And node evaluate result is:
    """
    : 2
    """

  Scenario: raiser error when parentheses has no data
    Given the following dal code:
    """
      ()
    """
    Then failed to get "parentheses" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
      ()
       ^
    """

  Scenario: raiser error when parentheses is not finished
    Given the following dal code:
    """
      (1
    """
    Then failed to get "parentheses" node with the following message:
    """
    should end with `)`
    """
    And got the following source code information:
    """
      (1
        ^
    """

  Scenario: raiser error when got unexpected token
    Given the following dal code:
    """
      (1 1
    """
    Then failed to get "parentheses" node with the following message:
    """
    should end with `)`
    """
    And got the following source code information:
    """
      (1 1
          ^
    """
