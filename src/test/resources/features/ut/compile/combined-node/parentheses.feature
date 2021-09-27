Feature: parentheses node

  Scenario: return null when does not match
    Given the following dal code xx:
    """
    not start with (
    """
    Then got the following "parentheses" node xx:
    """
    : null
    """

  Scenario: single value in parentheses
    Given the following dal code xx:
    """
     (1)
    """
    Then got the following "parentheses" node xx:
    """
    : {
      class.simpleName: 'ParenthesesNode'
      inspect: '(1)'
      positionBegin: 1
    }
    """
    And evaluate result is xx:
    """
    : 1
    """

  Scenario: expression in parentheses
    Given the following dal code xx:
    """
     (1+1)
    """
    Then got the following "parentheses" node xx:
    """
    : {
      class.simpleName: 'ParenthesesNode'
      inspect: '(1 + 1)'
      positionBegin: 1
    }
    """
    And evaluate result is xx:
    """
    : 2
    """

  Scenario: raiser error when parentheses has no data
    Given the following dal code xx:
    """
      ()
    """
    Then failed to get "parentheses" node with the following message xx:
    """
    expect a value or expression
    """
    And got the following source code information xx:
    """
      ()
       ^
    """

  Scenario: raiser error when parentheses is not finished
    Given the following dal code xx:
    """
      (1
    """
    Then failed to get "parentheses" node with the following message xx:
    """
    should end with `)`
    """
    And got the following source code information xx:
    """
      (1
        ^
    """

  Scenario: raiser error when got unexpected token
    Given the following dal code xx:
    """
      (1 1
    """
    Then failed to get "parentheses" node with the following message xx:
    """
    should end with `)`
    """
    And got the following source code information xx:
    """
      (1 1
          ^
    """
