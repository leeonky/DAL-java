Feature: bracket property node

  Scenario: return null when does not match
    Given the following dal code:
    """
    not [
    """
    Then got the following "bracket-property" node:
    """
    : null
    """

  Scenario: access list element
    Given the following dal code:
    """
      [1]
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[1]'
    }
    """
    When the following input data:
    """
      [0, 1]
    """
    Then evaluate result is:
    """
    : 1
    """

  Scenario Outline: access object property
    Given the following dal code:
    """
      [<name>]
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: "['first name']"
    }
    """
    When the following input data:
    """
      { "first name": "Tom" }
    """
    Then evaluate result is:
    """
    : 'Tom'
    """
    Examples:
      | name         |
      | 'first name' |
      | "first name" |

  Scenario: access from the end of list
    Given the following dal code:
    """
    [-1]
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[-1]'
    }
    """
    When the following input data:
    """
      [0, 1]
    """
    Then evaluate result is:
    """
    : 1
    """

  Scenario: white space in bracket
    Given the following dal code:
    """
    [ 100 ]
    """
    Then got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[100]'
    }
    """

  Scenario Outline: raise error when got invalid value in bracket
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
      | +    |
      |      |

  Scenario: raise error when more than one token in brackets
    Given the following dal code:
    """
    [1 2]
    """
    Then failed to get "bracket-property" node with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following source code information:
    """
    [1 2]
        ^
    """

  Scenario: raise error when missed closing ']'
    Given the following dal code:
    """
    [100
    """
    Then failed to get "bracket-property" node with the following message:
    """
    should end with `]`
    """
    And got the following source code information:
    """
    [100
        ^
    """

  Scenario: raise error when missed token and closing ']'
    Given the following dal code:
    """
    [
    """
    Then failed to get "bracket-property" node with the following message:
    """
    should end with `]`
    """
    And got the following source code information:
    """
    [
     ^
    """
