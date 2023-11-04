Feature: const number node

  Scenario Outline: supported format for number parsing
    When evaluate by:
    """
     <code>
    """
    Then the result should:
    """
    = <evaluate>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code              | evaluate        | inspect           |
      | 100               | 100             | 100               |
      | 99999999999999    | 99999999999999  | 99999999999999    |
      | 0x100             | 256             | 256               |
      | 1.1               | 1.1             | 1.1               |
      | 1e1               | 10.0            | 10.0              |
      | 1e+1              | 10.0            | 10.0              |
      | 1e-1              | 0.1             | 0.1               |
      | 1E1               | 10.0            | 10.0              |
      | 1E+1              | 10.0            | 10.0              |
      | 1E-1              | 0.1             | 0.1               |
      | 0xfL              | 15L             | 15                |
      | (-100)            | -100            | (-100)            |
      | (-99999999999999) | -99999999999999 | (-99999999999999) |
      | (-0x100)          | -256            | (-256)            |
      | (-1.1)            | -1.1            | (-1.1)            |
      | (-1e1)            | -10.0           | (-10.0)           |
      | (-1e+1)           | -10.0           | (-10.0)           |
      | (-1e-1)           | -0.1            | (-0.1)            |
      | (-1E1)            | -10.0           | (-10.0)           |
      | (-1E+1)           | -10.0           | (-10.0)           |
      | (-1E-1)           | -0.1            | (-0.1)            |
      | (-0xfL)           | -15L            | (-15)             |

  Scenario: record number position
    When evaluate by:
    """
    null: 1.14
    """
    Then got the following notation:
    """
    null: 1.14
          ^
    """

  Scenario: return empty when invalid number, and source code offset should rollback to beginning
    Given the following json:
    """
    {
      "12Invalid": "a string"
    }
    """
    When evaluate by:
    """
    12Invalid
    """
    Then the result should:
    """
    : 'a string'
    """

  Scenario Outline: delimiter between numbers
    When evaluate follow expression as "number" node:
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

  Scenario: get number before dot when whole token is not number
    * the following verification should pass:
    """
    '1': 1.toString
    """
    When evaluate by:
    """
    1.
    """
    Then failed with the message:
    """
    Expect a symbol
    """
    And got the following notation:
    """
    1.
      ^
    """
