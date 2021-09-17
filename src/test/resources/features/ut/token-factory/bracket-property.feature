Feature: bracket property token

  Scenario: return null when first char dose not match
    Given the following dal code:
    """
    not [
    """
    Then got the following "bracket-property" token:
    """
    : null
    """

  Scenario Outline: return null when after judgement operator
    Given the following dal code:
    """
    <judgement>[1]
    """
    Then got the following "bracket-property" token:
    """
    : null
    """
    Examples:
      | judgement |
      | :         |
      | =         |

  Scenario: raise error when got invalid value in bracket
    Given the following dal code:
    """
    [+]
    """
    Then failed to take "bracket-property" token with the following message:
    """
    Unexpected token
    """
    And got the following source code information:
    """
    [+]
     ^
    """

  Scenario: access list element
    Given the following dal code:
    """
    [100]=
    """
    Then got the following "bracket-property" token:
    """
    : {
      type: 'PROPERTY'
      value: 100
    }
    """
    And current offset char of source code is "="

  Scenario: access from the end of list
    Given the following dal code:
    """
    [-1]=
    """
    Then got the following "bracket-property" token:
    """
    : {
      type: 'PROPERTY'
      value: -1
    }
    """
    And current offset char of source code is "="

  Scenario: raise error when list index is not integer
    Given the following dal code:
    """
    [1.1]
    """
    Then failed to take "bracket-property" token with the following message:
    """
    must be integer
    """
    And got the following source code information:
    """
    [1.1]
     ^
    """

  Scenario Outline: access object property
    Given the following dal code:
    """
    [<code>]
    """
    When got the following "bracket-property" token:
    """
    : {
      type: 'PROPERTY'
      value: <value>
    }
    """
    Examples:
      | code   | value  |
      | "key1" | 'key1' |
      | 'key2' | 'key2' |

  Scenario: white space in bracket
    Given the following dal code:
    """
    [ 100 ]
    """
    When got the following "bracket-property" token:
    """
    : {
      type: 'PROPERTY'
      value: 100
    }
    """

  Scenario: raise error when no token in brackets
    Given the following dal code:
    """
    []
    """
    Then failed to take "bracket-property" token with the following message:
    """
    should given one property or array index in `[]`
    """
    And got the following source code information:
    """
    []
     ^
    """

  Scenario: raise error when more than one token in brackets
    Given the following dal code:
    """
    [1 2]
    """
    Then failed to take "bracket-property" token with the following message:
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
    Then failed to take "bracket-property" token with the following message:
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
    Then failed to take "bracket-property" token with the following message:
    """
    should end with `]`
    """
    And got the following source code information:
    """
    [
     ^
    """
