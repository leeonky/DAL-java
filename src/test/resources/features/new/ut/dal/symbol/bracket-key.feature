Feature: bracket symbol node

  Scenario: return null when does not match
    Given the following dal expression:
    """
    not [
    """
    Then parse the following "bracket-symbol" node:
    """
    : null
    """

  Scenario: access list element
    Given the following dal expression:
    """
      [1]
    """
    Then parse the following "bracket-symbol" node:
    """
    : {
      class.simpleName: 'SymbolNode'
      positionBegin: 2
      inspect: '[1]'
    }
    """
    When the following json:
    """
      [0, 1]
    """
    Then last evaluated node result is:
    """
    : 1
    """

  Scenario Outline: access object property
    Given the following dal expression:
    """
      [<name>]
    """
    Then parse the following "bracket-symbol" node:
    """
    : {
      class.simpleName: 'SymbolNode'
      positionBegin: 2
      inspect: "['first name']"
    }
    """
    When the following json:
    """
      { "first name": "Tom" }
    """
    Then last evaluated node result is:
    """
    : 'Tom'
    """
    Examples:
      | name         |
      | 'first name' |
      | "first name" |

  Scenario: access from the end of list
    Given the following dal expression:
    """
    [-1]
    """
    Then parse the following "bracket-symbol" node:
    """
    : {
      class.simpleName: 'SymbolNode'
      inspect: '[-1]'
    }
    """
    When the following json:
    """
      [0, 1]
    """
    Then last evaluated node result is:
    """
    : 1
    """

  Scenario: white space in bracket
    Given the following dal expression:
    """
    [ 100 ]
    """
    Then parse the following "bracket-symbol" node:
    """
    : {
      class.simpleName: 'SymbolNode'
      inspect: '[100]'
    }
    """

  Scenario Outline: raise error when got invalid value in bracket
    Given the following dal expression:
    """
    [<code>]
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following notation:
    """
    [<code>]
     ^
    """
    Examples:
      | code |
      | +    |
      |      |

  Scenario: raise error when more than one token in brackets
    Given the following dal expression:
    """
    [1 2]
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following notation:
    """
    [1 2]
        ^
    """

  Scenario: raise error when missed closing ']'
    Given the following dal expression:
    """
    [100
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should end with `]`
    """
    And got the following notation:
    """
    [100
        ^
    """

  Scenario: raise error when missed token and closing ']'
    Given the following dal expression:
    """
    [
    """
    Then failed to parse "bracket-symbol" with the following message:
    """
    should end with `]`
    """
    And got the following notation:
    """
    [
     ^
    """
