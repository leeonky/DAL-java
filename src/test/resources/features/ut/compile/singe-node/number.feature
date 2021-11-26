Feature: const number node

  Scenario: null when does not match
    Given the following dal code:
    """
    not starts with digital
    """
    Then got the following "number" node:
    """
    : null
    """

  Scenario Outline: supported format for number parsing
    Given the following dal code:
    """
     <code>
    """
    Then got the following "number" node:
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
      | 1.1            | 1.1            | 1.1            |

  Scenario Outline: delimiter between numbers
    Given the following dal code:
    """
     1<delimiter>
    """
    Then node evaluate as "number" result is:
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
    Given the following dal code:
    """
    12Invalid
    """
    Then got the following "number" node:
    """
    : null
    """
    Then got the following "identity-property" node:
    """
    : {
      inspect: '12Invalid'
    }
    """

  Scenario: supported hex integer in any case
    Given the following input java class data:
    """
    public class Numbers {
      public long value = 15;
    }
    """
    Then the following assertion for "Numbers" should pass:
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
    * the following assertion should pass:
    """
      1e1: 10.0,
      1e+1: 10.0,
      1e-1: 0.1,
      1E1: 10.0,
      1E+1: 10.0,
      1E-1: 0.1
    """
