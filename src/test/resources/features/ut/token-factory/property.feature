Feature: property token

  Scenario: return empty when not start with dot
    Given the following dal code:
    """
    not start with .
    """
    Then got the following "property" token:
    """
    : null
    """

  Scenario Outline: trim white space after dot
    Given the following dal code:
    """
    .<white-space>a
    """
    Then got the following "property" token:
    """
    : {
      type: 'PROPERTY'
      value: 'a'
    }
    """
    Examples:
      | white-space |
      | \n          |
      | `TAB        |
      | `SPACE      |

  Scenario Outline: property delimiter
    Given the following dal code:
    """
    .a<delimiter>
    """
    Then got the following "property" token:
    """
    : {
      type: 'PROPERTY'
      value: 'a'
    }
    """
    And current offset char of source code is "<delimiter>"
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

  Scenario: at the end of code
    Given the following dal code:
    """
    .a
    """
    Then got the following "property" token:
    """
    : {
      type: 'PROPERTY'
      value: 'a'
    }
    """

  Scenario: do not allow empty property
    Given the following dal code:
    """
    .
    """
    Then failed to take "property" token with the following message:
    """
    property not finished
    """
    And got the following source code information:
    """
    .
    ^
    """

  Scenario: do not allow empty property(has white space)
    Given the following dal code:
    """
    .   
    """
    Then failed to take "property" token with the following message:
    """
    property not finished
    """
    And got the following source code information:
    """
    .   
       ^
    """

