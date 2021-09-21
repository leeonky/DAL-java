Feature: compile all nodes with delimiter

  Scenario: compile all nodes with delimiter
    Given the following dal code xx:
    """
    1 'hello' "world" /regex/ 2
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
    And got the following "number" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '2'
    }
    """
