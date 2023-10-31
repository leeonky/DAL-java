Feature: integer node

  Scenario: null when does not match
    Given the following dal expression:
    """
    not starts with digital
    """
    Then parse the following "integer" node:
    """
    : null
    """

  Scenario Outline: supported format for integer parsing
    Given the following dal expression:
    """
     <code>
    """
    Then parse the following "integer" node:
    """
    : {
      class.simpleName: 'ConstValueNode'
      inspect: '<inspect>'
      positionBegin: 1
    }
    """
    And last evaluated node result is:
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
    When evaluate follow expression as "integer" node:
    """
     1<delimiter>
    """
    Then the result should:
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
    Given the following dal expression:
    """
    1.1
    """
    Then failed to parse "integer" with the following message:
    """
    expect an integer
    """
