Feature: "string"

  Scenario: return empty when first char is not matched
    Given the following dal code xx:
    """
    not starts with "
    """
    Then got the following "const" node xx:
    """
    : null
    """

  Scenario Outline: double quoted string
    Given the following dal code xx:
    """
    "<str>"
    """
    Then got the following "const" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '<inspect>'
      positionBegin: 0
    }
    """
    Examples:
      | str   | inspect   |
      |       | \'\'      |
      | hello | \'hello\' |
