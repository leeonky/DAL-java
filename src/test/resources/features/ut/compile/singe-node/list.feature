Feature: list node

  Scenario: return null when does not match
    Given the following dal code:
    """
    +
    """
    Then got the following "list" node:
    """
    : null
    """

  Scenario: support empty list
    Given the following dal code:
    """
     []
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[]'
      positionBegin: 1
    }
    """

  Scenario: support one element list
    Given the following dal code:
    """
     [1]
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[1]'
      positionBegin: 1
      expressions.inspect: ['[0] : 1']
    }
    """

  Scenario: support two elements list
    Given the following dal code:
    """
     [1 2]
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[1 2]'
      positionBegin: 1
      expressions.inspect: [
        '[0] : 1'
        '[1] : 2'
      ]
    }
    """

  Scenario: raise error when no closing bracket
    Given the following dal code:
    """
    [1
    """
    Then failed to get "list" node with the following message:
    """
    should end with `]`
    """
    And got the following source code information:
    """
    [1
      ^
    """

  Scenario: raise error when element is invalid
    Given the following dal code:
    """
    [ + ]
    """
    Then failed to get "list" node with the following message:
    """
    expect a value or expression
    """
    And got the following source code information:
    """
    [ + ]
      ^
    """

  Scenario: support incomplete List
    Given the following dal code:
    """
     [1 ...]
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[1 ...]'
      positionBegin: 1
      expressions.inspect: [
        '[0] : 1'
      ]
    }
    """

  Scenario Outline: support different judgement operator in element
    Given the following dal code:
    """
     [<operator>1]
    """
    Then got the following "list" node:
    """
    expressions.inspect: [
      '[0] <result> 1'
    ]
    """
    Examples:
      | operator | result |
      |          | :      |
      | =        | =      |
      | :        | :      |

  Scenario Outline: support different judgement operator in sub element
    Given the following dal code:
    """
     [<operator>[1]]
    """
    Then got the following "list" node:
    """
    expressions[0].rightOperand.expressions.inspect: [
      '[0] <result> 1'
    ]
    """
    Examples:
      | operator | result |
      |          | :      |
      | =        | =      |
      | :        | :      |

  Scenario Outline: support expression in element judgement
    Given the following dal code:
    """
     [<operator>2+1]
    """
    Then got the following "list" node:
    """
    expressions[0]: {
      leftOperand.inspect: '[0]'
      operator.class.simpleName: '<result>'
      rightOperand.inspect: '2 + 1'
    }
    """
    Examples:
      | operator | result  |
      |          | Matcher |
      | =        | Equal   |
      | :        | Matcher |

  Scenario: support optional comma between elements
    Given the following dal code:
    """
     [true, false]
    """
    Then got the following "list" node:
    """
    inspect: '[true false]'
    """

  Scenario: comma as and in parentheses
    Given the following dal code:
    """
     [(a,b), c]
    """
    Then got the following "list" node:
    """
    inspect: '[(a , b) c]'
    """

  Scenario: support optional comma before bracket
    Given the following dal code:
    """
     [true,]
    """
    Then got the following "list" node:
    """
    inspect: '[true]'
    """

  Scenario: blank before bracket
    Given the following dal code:
    """
     [true, ]
    """
    Then got the following "list" node:
    """
    inspect: '[true]'
    """
