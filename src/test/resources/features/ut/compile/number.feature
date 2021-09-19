Feature: const number node

#TODO support different type of number
#TODO null
  Scenario: null when does not match
    Given the following dal code xx:
    """
    not starts with digital
    """
    Then got the following "const" node xx:
    """
    : null
    """

  Scenario Outline: number
    Given the following dal code xx:
    """
     <code>
    """
    Then got the following "const" node xx:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '<inspect>'
      positionBegin: 1
    }
    """
    And evaluate result is xx:
    """
    : <evaluate>
    """
    Examples:
      | code  | inspect | evaluate |
      | 100   | 100     | 100      |
      | 0x100 | 256     | 256      |
      | 1.1   | 1.1     | 1.1      |

  Scenario Outline: delimiter between numbers
    Given the following dal code xx:
    """
     1<delimiter>
    """
    Then got the following "const" node xx:
    """
    : {
      inspect: '1'
      positionBegin: 1
    }
    """
    Examples:
      | delimiter |
      | (         |
      | )         |
      | =         |
      | >         |
      | <         |
      | +         |
      | -         |
      | *         |
      | /         |
      | &         |
      | !         |
      | ,         |
      | [         |
      | ]         |
      | :         |
      | \|        |
      | \n        |
      | `TAB      |
      | `SPACE    |
