Feature: bracket property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    100
    """
    Then got the following "bracket-property" node:
    """
    : null
    """

  Scenario: compile access list as property node
    Given the following dal code:
    """
      [100]
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[100]'
    }
    """

  Scenario: access object property
    Given the following dal code:
    """
      ['first name']
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: "['first name']"
    }
    """

  Scenario Outline: invalid token in []
    Given the following dal code:
    """
      [<code>]
    """
    Then failed to get "bracket-property" node with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following source code information:
    """
      [<code>]
       ^
    """
    Examples:
      | code |
      |      |
      | +    |

  Scenario: more than one value in []
    Given the following dal code:
    """
      [1 2]
    """
    Then failed to get "bracket-property" node with the following message:
    """
    unexpected token, `]` expected
    """
    And got the following source code information:
    """
      [1 2]
         ^
    """

  Scenario: missing closing ]
    Given the following dal code:
    """
      [1
    """
    Then failed to get "bracket-property" node with the following message:
    """
    missed `]`
    """
    And got the following source code information:
    """
      [1
        ^
    """

  Scenario: missing value and closing ]
    Given the following dal code:
    """
      [
    """
    Then failed to get "bracket-property" node with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following source code information:
    """
      [
       ^
    """
