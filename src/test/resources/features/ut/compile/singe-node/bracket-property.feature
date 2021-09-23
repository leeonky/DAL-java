Feature: bracket property node

  Scenario: return null when does not match
    Given the following dal code xx:
    """
    not [
    """
    Then got the following "bracket-property" node xx:
    """
    : null
    """

  Scenario: access list element
    Given the following dal code xx:
    """
      [1]
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '[1]'
    }
    """
    When the following input data xx:
    """
      [0, 1]
    """
    Then evaluate result is xx:
    """
    : 1
    """

  Scenario Outline: access object property
    Given the following dal code xx:
    """
      [<name>]
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: "['first name']"
    }
    """
    When the following input data xx:
    """
      { "first name": "Tom" }
    """
    Then evaluate result is xx:
    """
    : 'Tom'
    """
    Examples:
      | name         |
      | 'first name' |
      | "first name" |

  Scenario: access from the end of list
    Given the following dal code xx:
    """
    [-1]
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[-1]'
    }
    """
    When the following input data xx:
    """
      [0, 1]
    """
    Then evaluate result is xx:
    """
    : 1
    """

  Scenario: white space in bracket
    Given the following dal code xx:
    """
    [ 100 ]
    """
    Then got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[100]'
    }
    """

  Scenario Outline: raise error when got invalid value in bracket
    Given the following dal code xx:
    """
    [<code>]
    """
    Then failed to get "bracket-property" node with the following message xx:
    """
    should given one property or array index in `[]`
    """
    And got the following source code information xx:
    """
    [<code>]
     ^
    """
    Examples:
      | code |
      | +    |
      |      |

  Scenario: raise error when list index is not integer
    Given the following dal code xx:
    """
    [1.1]
    """
    Then failed to get "bracket-property" node with the following message xx:
    """
    index must be integer
    """
    And got the following source code information xx:
    """
    [1.1]
     ^
    """

  Scenario: raise error when more than one token in brackets
    Given the following dal code xx:
    """
    [1 2]
    """
    Then failed to get "bracket-property" node with the following message xx:
    """
    should given one property or array index in `[]`
    """
    And got the following source code information xx:
    """
    [1 2]
        ^
    """

  Scenario: raise error when missed closing ']'
    Given the following dal code xx:
    """
    [100
    """
    Then failed to get "bracket-property" node with the following message xx:
    """
    should end with `]`
    """
    And got the following source code information xx:
    """
    [100
        ^
    """

  Scenario: raise error when missed token and closing ']'
    Given the following dal code xx:
    """
    [
    """
    Then failed to get "bracket-property" node with the following message xx:
    """
    should end with `]`
    """
    And got the following source code information xx:
    """
    [
     ^
    """
