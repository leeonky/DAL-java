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
      inspect: '[: 1]'
      positionBegin: 1
      expressions.inspect: ['[0]: 1']
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
      inspect: '[: 1, : 2]'
      positionBegin: 1
      expressions.inspect: [
        '[0]: 1'
        '[1]: 2'
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
      inspect: '[: 1, ...]'
      positionBegin: 1
      expressions.inspect: [
        '[0]: 1'
      ]
    }
    """

  Scenario: support match list from last index
    Given the following dal code:
    """
     [... 1]
    """
    Then got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[..., : 1]'
      positionBegin: 1
      expressions.inspect: [
        '[-1]: 1'
      ]
    }
    """

  Scenario: should raise error when invalid incomplete List
    Given the following dal code:
    """
     [... 1 ... 2]
    """
    Then failed to get "list" node with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
     [... 1 ... 2]
            ^
    """
    Given the following dal code:
    """
     [0 ... 1 ...]
    """
    Then failed to get "list" node with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
     [0 ... 1 ...]
        ^
    """
    Given the following dal code:
    """
     [0 ... 1]
    """
    Then failed to get "list" node with the following message:
    """
    unexpected token
    """
    And got the following source code information:
    """
     [0 ... 1]
        ^
    """

  Scenario Outline: support different judgement operator in element
    Given the following dal code:
    """
     [<operator>1]
    """
    Then got the following "list" node:
    """
    expressions.inspect: [
      '[0]<result> 1'
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
      '[0]<result> 1'
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
    inspect: '[: true, : false]'
    """

  Scenario: comma as and in parentheses
    Given the following dal code:
    """
     [(a,b), c]
    """
    Then got the following "list" node:
    """
    inspect: '[: (a , b), : c]'
    """

  Scenario: support optional comma before bracket
    Given the following dal code:
    """
     [true,]
    """
    Then got the following "list" node:
    """
    inspect: '[: true]'
    """

  Scenario: blank before bracket
    Given the following dal code:
    """
     [true, ]
    """
    Then got the following "list" node:
    """
    inspect: '[: true]'
    """

  Scenario: support schema expression
    Given the following dal code:
    """
     [is Schema]
    """
    Then got the following "list" node:
    """
    inspect: '[is Schema]'
    expressions[0]: {
      class.simpleName: 'SchemaExpression'
      inspect: '[0] is Schema'
    }
    """

  Scenario: support schema in judgement expression
    Given the following dal code:
    """
     [ is String: 'hello' ]
    """
    Then got the following "list" node:
    """
    inspect: "[is String: 'hello']"
    expressions[0]: {
      class.simpleName: 'Expression'
      leftOperand: {
        class.simpleName: 'SchemaExpression'
        inspect: '[0] is String'
      }
      operator.class.simpleName: 'Matcher'
      rightOperand: {
        class.simpleName: 'ConstNode'
        inspect: "'hello'"
      }
    }
    """

  Scenario: support user define first element index
    Given the following input java class data:
    """
    public class Table extends java.util.ArrayList<Row> {
      public Table() {
        Row row = new Row();
        row.v1 = 11;
        row.v2 = 12;
        add(row);

        row = new Row();
        row.v1 = 21;
        row.v2 = 22;
        add(row);
      }
    }
    """
    And the following input java class data:
    """
    public class Row {
      public int v1, v2;
    }
    """
    And set the first element index to 1 of list type "Table"
    And the following assertion for "Table" should pass:
    """
    : {
      [1]: {
        v1: 11
        v2: 12
      }
      [2]: {
        v1: 21
        v2: 22
      }
    }
    """
    And the following assertion for "Table" should pass:
    """
    [-2]: {
      v1: 11
      v2: 12
    }
    """
    And the following assertion for "Table" should pass:
    """
    : [{
      v1: 11
      v2: 12
    }{
      v1: 21
      v2: 22
    }]
    """
    And the following assertion for "Table" should pass:
    """
    : [{
      v1: 11
      v2: 12
    } ...]
    """
    And the following assertion for "Table" should pass:
    """
    : [... {
      v1: 21
      v2: 22
    }]
    """
    And the following assertion for "Table" should pass:
    """
    : | v1 | v2 |
      | 11 | 12 |
      | 21 | 22 |
    """
    And the following assertion for "Table" should pass:
    """
    : >>| v1 | 11 | 21 |
        | v2 | 12 | 22 |
    """
