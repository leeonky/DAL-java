Feature: integer node

  Scenario: null when does not match
    Given the following dal code:
    """
    not starts with digital
    """
    Then got the following "integer" node:
    """
    : null
    """

  Scenario Outline: supported format for integer parsing
    Given the following dal code:
    """
     <code>
    """
    Then got the following "integer" node:
    """
    : {
      class.simpleName: 'ConstNode'
      inspect: '<inspect>'
      positionBegin: 1
    }
    """
    And node evaluate result is:
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
    Given the following dal code:
    """
     1<delimiter>
    """
    Then node evaluate as "integer" result is:
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
    Given the following dal code:
    """
    1.1
    """
    Then failed to get "integer" node with the following message:
    """
    expect an integer
    """
