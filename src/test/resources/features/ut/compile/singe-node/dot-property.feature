Feature: compile property starts with dot

  Scenario: return null when not start with dot
    Given the following dal code:
    """
    not start with .
    """
    Then got the following "dot-property" node:
    """
    : null
    """

  Scenario: compile property starts with dot
    Given the following dal code:
    """
      .name
    """
    Then got the following "dot-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '.name'
    }
    """
    When the following input data:
    """
      {"name": "Tom"}
    """
    Then evaluate result is:
    """
      : 'Tom'
    """

  Scenario Outline: trim white space after dot
    Given the following dal code:
    """
    .<white-space>a
    """
    Then got the following "dot-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '.a'
    }
    """
    Examples:
      | white-space |
      | \n          |
      | `TAB        |
      | `SPACE      |

  Scenario Outline: property before delimiter and dot
    Given the following dal code:
    """
    .a<delimiter>b
    """
    Then got the following "dot-property" node:
    """
    : {
      class.simpleName: 'PropertyNode'
      inspect: '.a'
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
      | .         |

  Scenario: do not allow empty property
    Given the following dal code:
    """
    .
    """
    Then failed to get "dot-property" node with the following message:
    """
    property is not finished
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
    Then failed to get "dot-property" node with the following message:
    """
    property is not finished
    """
    And got the following source code information:
    """
    . 
    ^
    """

  Scenario: return null when code is ...
    Given the following dal code:
    """
    ...
    """
    Then got the following "dot-property" node:
    """
    : null
    """
