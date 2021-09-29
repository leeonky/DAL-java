Feature: compile all nodes with delimiter

  Scenario: compile all nodes with delimiter
    Given the following dal code xx:
    """
    1 'hello' "world" /regex/ true false null identityProperty [1] 1a  [1] {a: 1} 2
    """
    Then got the following "number" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '1'
    }
    """
    And got the following "single-quoted-string" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: "'hello'"
    }
    """
    And got the following "double-quoted-string" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: "'world'"
    }
    """
    And got the following "regex" node xx:
    """
    : {
      class.simpleName: 'RegexNode'
      inspect: '/regex/'
    }
    """
    And got the following "const-true" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'true'
    }
    """
    And got the following "const-false" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'false'
    }
    """
    And got the following "const-null" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: 'null'
    }
    """
    And got the following "identity-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: 'identityProperty'
    }
    """
    And got the following "bracket-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '[1]'
    }
    """
    And got the following "identity-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '1a'
    }
    """
    And got the following "list" node xx:
    """
    : {
      class.simpleName: 'ListNode'
      inspect: '[1]'
    }
    """
    And got the following "object" node xx:
    """
    : {
      class.simpleName: 'ObjectNode'
      inspect: '{a : 1}'
    }
    """
    And got the following "number" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '2'
    }
    """
