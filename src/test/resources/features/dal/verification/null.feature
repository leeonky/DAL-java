Feature: compare null

  Scenario Outline: non null equals to null
    Given the following json:
    """
      <value>
    """
    When evaluate by:
    """
      = null
    """
    Then failed with the message:
    """
    Expected to be equal to: null
                             ^
    Actual: <type>
            ^
    <<message>>
    """
    Examples:
      | value | type              | message |
      | 0     | java.lang.Integer | 0       |
      | ""    | java.lang.String  |         |

  Scenario: empty list equals to null
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
      = null
    """
    Then failed with the message:
    """
    Expected to be equal to: null
                             ^
    Actual: []
            ^
    """

  Scenario: empty map equals to null
    Given the following json:
    """
    {}
    """
    When evaluate by:
    """
      = null
    """
    Then failed with the message:
    """
    Expected to be equal to: null
                             ^
    Actual: {}
            ^
    """

  Scenario Outline: non null equals to null
    Given the following json:
    """
      <value>
    """
    When evaluate by:
    """
      = null
    """
    Then failed with the message:
    """
    Expected to be equal to: null
                             ^
    Actual: <type>
            ^
    <<message>>
    """
    Examples:
      | value | type              | message |
      | 0     | java.lang.Integer | 0       |
      | ""    | java.lang.String  |         |

  Scenario: empty list matches null
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
      : null
    """
    Then failed with the message:
    """
    Expected to match: null
                       ^
    Actual: []
            ^
    """

  Scenario: empty map matches null
    Given the following json:
    """
    {}
    """
    When evaluate by:
    """
      : null
    """
    Then failed with the message:
    """
    Expected to match: null
                       ^
    Actual: {}
            ^
    """

  Scenario Outline: null equals to non null
    When evaluate by:
    """
      null = <value>
    """
    Then failed with the message:
    """
    Expected to be equal to: <type>
                             ^
    <<message>>
    Actual: null
            ^
    """
    And got the following notation:
    """
      null = <value>
             ^
    """
    Examples:
      | value | message | type              |
      | 0     | 0       | java.lang.Integer |
      | ""    |         | java.lang.String  |

  Scenario Outline: null matches non null
    When evaluate by:
    """
    null: <value>
    """
    Then failed with the message:
    """
    Expected to match: <type>
                       ^
    <<message>>
    Actual: null
            ^
    """
    Examples:
      | value | message | type              |
      | 0     | 0       | java.lang.Integer |
      | ""    |         | java.lang.String  |

  Scenario Outline: compare null and list
    When evaluate by:
    """
      null <operator> []
    """
    Then failed with the message:
    """
    Invalid input value, expect a List but: null
    """
    And got the following notation:
    """
      null <operator> []
      ^
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: compare null and object
    When evaluate by:
    """
      null <operator> <empty>
    """
    Then failed with the message:
    """
    The input value is null
    """
    And got the following notation:
    """
      null <operator> <empty>
             ^
    """
    Examples:
      | operator | empty |
      | =        | {}    |
      | :        | {...} |

  Scenario Outline: compare null and null
    Given the following json:
    """
      {
        "value": null
      }
    """
    Then the following verification should pass:
    """
      value <operator> null and null <operator> .value
    """
    Examples:
      | operator |
      | =        |
      | :        |
