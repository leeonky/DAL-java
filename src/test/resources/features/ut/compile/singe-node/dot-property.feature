Feature: compile property starts with dot

  Scenario: return null when not start with dot
    Given the following dal code xx:
    """
    not start with .
    """
    Then got the following "dot-property" node xx:
    """
    : null
    """

  Scenario: compile property starts with dot
    Given the following dal code xx:
    """
      .name
    """
    Then got the following "dot-property" node xx:
    """
    : {
      class.simpleName: 'PropertyNode'
      positionBegin: 2
      inspect: '.name'
    }
    """
    Given the following input data xx:
    """
      {"name": "Tom"}
    """
    Then the following assertion should pass xx:
    """
      .name = 'Tom'
    """

  Scenario Outline: trim white space after dot
    Given the following dal code xx:
    """
    .<white-space>a
    """
    Then got the following "dot-property" node xx:
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
    Given the following dal code xx:
    """
    .a<delimiter>b
    """
    Then got the following "dot-property" node xx:
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
    Given the following dal code xx:
    """
    .
    """
    Then failed to get "dot-property" node with the following message xx:
    """
    property is not finished
    """
    And got the following source code information xx:
    """
    .
     ^
    """

  Scenario: do not allow empty property(has white space)
    Given the following dal code xx:
    """
    . 
    """
    Then failed to get "dot-property" node with the following message xx:
    """
    property is not finished
    """
    And got the following source code information xx:
    """
    . 
      ^
    """
