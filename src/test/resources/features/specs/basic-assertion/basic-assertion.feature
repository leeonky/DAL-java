Feature: basic assertion

  Scenario Outline: non null equals to null
    Given the following input data:
    """
      <value>
    """
    When assert by the following code:
    """
      = null
    """
    Then failed with the following message:
    """
    expected <type>
    <<message>>
    equals to null but was not
    """
    Examples:
      | value | type                | message |
      | 0     | java.lang.Integer   | 0       |
      | ""    | java.lang.String    |         |
      | {}    | org.json.JSONObject | {}      |
      | []    | org.json.JSONArray  | []      |

  Scenario Outline: non null matches null
    Given the following input data:
    """
      <value>
    """
    When assert by the following code:
    """
      : null
    """
    Then failed with the following message:
    """
    expected <type>
    <<message>>
    matches null but was not
    """
    Examples:
      | value | type                | message |
      | 0     | java.lang.Integer   | 0       |
      | ""    | java.lang.String    |         |
      | {}    | org.json.JSONObject | {}      |
      | []    | org.json.JSONArray  | []      |

  Scenario Outline: null equals to non null
    When assert by the following code:
    """
      null = <value>
    """
    Then failed with the following message:
    """
    expected null equals to <type>
    <<message>>
    but was not
    """
    And got the following source code information:
    """
      null = <value>
             ^
    """
    Examples:
      | value | message | type              |
      | 0     | 0       | java.lang.Integer |
      | ""    |         | java.lang.String  |

  Scenario Outline: null matches non null
    When assert by the following code:
    """
    null: <value>
    """
    Then failed with the following message:
    """
    expected null matches <type>
    <<message>>
    but was not
    """
    Examples:
      | value | message | type              |
      | 0     | 0       | java.lang.Integer |
      | ""    |         | java.lang.String  |

  Scenario Outline: compare null and list
    When assert by the following code:
    """
      null <operator> []
    """
    Then failed with the following message:
    """
    cannot compare null and list
    """
    And got the following source code information:
    """
      null <operator> []
             ^
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: compare null and object
    When assert by the following code:
    """
      null <operator> {}
    """
    Then failed with the following message:
    """
    the input value is null
    """
    And got the following source code information:
    """
      null <operator> {}
             ^
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: compare null and null
    Given the following input data:
    """
      {
        "value": null
      }
    """
    Then the following assertion should pass:
    """
      value <operator> null and null <operator> value
    """
    Examples:
      | operator |
      | =        |
      | :        |

  Scenario Outline: always pass
    Then the following assertion should pass:
    """
      <operand> : * and <operand> = *
    """
    Examples:
      | operand |
      | 1       |
      | ''      |
      | null    |
