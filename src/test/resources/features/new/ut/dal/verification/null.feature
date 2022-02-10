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
    Expecting <type>
    <<message>>
    to be equal to null but was not
    """
    Examples:
      | value | type                    | message |
      | 0     | java.lang.Integer       | 0       |
      | ""    | java.lang.String        |         |
      | {}    | java.util.LinkedHashMap | {}      |
      | []    | java.util.ArrayList     | []      |

  Scenario Outline: non null matches null
    Given the following json:
    """
      <value>
    """
    When evaluate by:
    """
      : null
    """
    Then failed with the message:
    """
    Expecting <type>
    <<message>>
    to match null but was not
    """
    Examples:
      | value | type                    | message |
      | 0     | java.lang.Integer       | 0       |
      | ""    | java.lang.String        |         |
      | {}    | java.util.LinkedHashMap | {}      |
      | []    | java.util.ArrayList     | []      |

  Scenario Outline: null equals to non null
    When evaluate by:
    """
      null = <value>
    """
    Then failed with the message:
    """
    Expecting null to be equal to <type>
    <<message>>
    but was not
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
    Expecting null to match <type>
    <<message>>
    but was not
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
    Cannot compare null and list
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
      null <operator> {}
    """
    Then failed with the message:
    """
    The input value is null
    """
    And got the following notation:
    """
      null <operator> {}
             ^
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: compare null and null
    Given the following json:
    """
      {
        "value": null
      }
    """
    Then the following verification should pass:
    """
      value <operator> null and null <operator> value
    """
    Examples:
      | operator |
      | =        |
      | :        |
