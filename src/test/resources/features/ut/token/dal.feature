Feature: dal token

  Scenario: parse all dal tokens:  [ ] .id 100 'str1'"str2" : /abc/ ( ) which { }
    Given the following dal code:
    """
    [ ] .id 100 'str1' "str2" : /abc/ ( ) which { }
    """
    Then got the following "dal" token:
    """
    : {
      type: 'TREE'
      tokenStream.tokens: [{
        type: 'OPENING_BRACKET'
        value: '['
        positionBegin: 0
        positionEnd: 1
      }
      {
        type: 'CLOSING_BRACKET'
        value: ']'
        positionBegin: 2
        positionEnd: 3
      }
      {
        type: 'PROPERTY'
        value: 'id'
        positionBegin: 4
        positionEnd: 7
      }
      {
        type: 'CONST_VALUE'
        value: 100
        positionBegin: 8
        positionEnd: 11
      }
      {
        type: 'CONST_VALUE'
        value: 'str1'
        positionBegin: 12
        positionEnd: 18
      }
      {
        type: 'CONST_VALUE'
        value: 'str2'
        positionBegin: 19
        positionEnd: 25
      }
      {
        type: 'OPERATOR'
        value: ':'
        positionBegin: 26
        positionEnd: 27
      }
      {
        type: 'REGEX'
        value: 'abc'
        positionBegin: 28
        positionEnd: 33
      }
      {
        type: 'OPENING_PARENTHESIS'
        value: '('
        positionBegin: 34
        positionEnd: 35
      }
      {
        type: 'CLOSING_PARENTHESIS'
        value: ')'
        positionBegin: 36
        positionEnd: 37
      }
      {
        type: 'KEY_WORD'
        value: 'which'
        positionBegin: 38
        positionEnd: 43
      }
      {
        type: 'OPENING_BRACE'
        value: '{'
        positionBegin: 44
        positionEnd: 45
      }
      {
        type: 'CLOSING_BRACE'
        value: '}'
        positionBegin: 46
        positionEnd: 47
      }]
    }
    """

  Scenario: return empty when no code
    Given the following dal code:
    """

    """
    Then got the following "dal" token:
    """
    : {
      type: 'TREE'
      tokenStream.tokens: []
    }
    """

  Scenario: return empty when only white space
    Given the following dal code:
    """


    """
    Then got the following "dal" token:
    """
    : {
      type: 'TREE'
      tokenStream.tokens: []
    }
    """
