Feature: integer node

  Scenario: null when does not match
    Given the following dal code xx:
    """
    not starts with digital
    """
    Then got the following "integer" node xx:
    """
    : null
    """

  Scenario Outline: supported format for integer parsing
    Given the following dal code xx:
    """
     <code>
    """
    Then got the following "integer" node xx:
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
      | code           | inspect        | evaluate       |
      | 100            | 100            | 100            |
      | 99999999999999 | 99999999999999 | 99999999999999 |
      | 0x100          | 256            | 256            |
      | -10            | -10            | -10            |

  Scenario Outline: delimiter between numbers
    Given the following dal code xx:
    """
     1<delimiter>
    """
    Then evaluate as "integer" result is:
    """
    : 1
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

  Scenario: raise error when number is not a integer
    Given the following dal code xx:
    """
    1.1
    """
    Then failed to get "integer" node with the following message xx:
    """
    expect an integer
    """
