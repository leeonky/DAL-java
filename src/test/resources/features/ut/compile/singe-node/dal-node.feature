Feature: compile all nodes with delimiter

  Scenario: compile all nodes with delimiter
    Given the following dal code:
    """
    1 'hello' "world" /regex/ true false null identityProperty [1] 1a  [1] {a: 1} a.b 2
    """
    Then got the following "number" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '1'
    }
    """
    And got the following "single-quoted-string" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: "'hello'"
    }
    """
    And got the following "double-quoted-string" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: "'world'"
    }
    """
    And got the following "regex" node:
    """
    : {
      class.simpleName: 'RegexNode'
      inspect: '/regex/'
    }
    """
    And got the following "const-true" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'true'
    }
    """
    And got the following "const-false" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'false'
    }
    """
    And got the following "const-null" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'null'
    }
    """
    And got the following "identity-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: 'identityProperty'
    }
    """
    And got the following "bracket-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[1]'
    }
    """
    And got the following "identity-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '1a'
    }
    """
    And got the following "list" node:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[: 1]'
    }
    """
    And got the following "object" node:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: '{a: 1}'
    }
    """
    And got the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaNode'
      inspect: 'a.b'
    }
    """
    And got the following "number" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '2'
    }
    """
