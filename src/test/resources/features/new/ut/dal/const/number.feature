Feature: const number node

  Scenario: null when does not match
    Given the following dal expression:
    """
    not starts with digital
    """
    Then parse the following "number" node:
    """
    : null
    """

  Scenario Outline: supported format for number parsing
    Given the following dal expression:
    """
     <code>
    """
    Then parse the following "number" node:
    """
    : {
      class.simpleName: 'ConstNode'
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
      | 1.1            | 1.1            | 1.1            |

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

  Scenario: return empty when invalid number, and source code offset should rollback to beginning
    Given the following dal expression:
    """
    12Invalid
    """
    Then parse the following "number" node:
    """
    : null
    """
    Then parse the following "symbol" node:
    """
    : {
      inspect: '12Invalid'
    }
    """

  Scenario: supported hex integer in any case
    Given the following java class:
    """
    public class Numbers {
      public long value = 15;
    }
    """
    Then the following verification for the instance of java class "Numbers" should pass:
    """
    : {
      value = 0xfl
      value = 0xfL
      value = 0xFl
      value = 0xFL
      value = 0Xfl
      value = 0XfL
      value = 0XFl
      value = 0XFL
    }
    """

  Scenario: parse float number
    * the following verification should pass:
    """
      1e1: 10.0,
      1e+1: 10.0,
      1e-1: 0.1,
      1E1: 10.0,
      1E+1: 10.0,
      1E-1: 0.1
    """
