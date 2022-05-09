Feature: boolean

  Scenario Outline: compare boolean
    Then the following verification should pass:
    """
      true<opt> true and
      false<opt> false
    """
    And the following verification should failed:
    """
      false<opt> true
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: different boolean with =
    Given evaluate by:
    """
      true= false
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.Boolean
    <false>
    Actual: java.lang.Boolean
    <true>
    """
    And got the following notation:
    """
      true= false
            ^
    """

  Scenario: different boolean with :
    Given evaluate by:
    """
      true: false
    """
    Then failed with the message:
    """
    Expected to match: java.lang.Boolean
    <false>
    Actual: java.lang.Boolean
    <true>
    """
    And got the following notation:
    """
      true: false
            ^
    """

  Scenario: not boolean type with =
    When evaluate by:
    """
      'true'= true
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.Boolean
    <true>
    Actual: java.lang.String
    <true>
    """
    And got the following notation:
    """
      'true'= true
              ^
    """

  Scenario: not allow convert string to boolean implicitly
    When evaluate by:
    """
      'true': true
    """
    Then failed with the message:
    """
    Cannot compare between java.lang.String
    <true>
    and java.lang.Boolean
    <true>
    """
    And got the following notation:
    """
      'true': true
            ^
    """

#    TODO object convert to Boolean