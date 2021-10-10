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
    equal to null but was not
    """
    Examples:
      | value | type                | message |
      | 0     | java.lang.Integer   | 0       |
      | ""    | java.lang.String    | ''      |
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
      | ""    | java.lang.String    | ''      |
      | {}    | org.json.JSONObject | {}      |
      | []    | org.json.JSONArray  | []      |

  Scenario Outline: null equals to non null
    When assert by the following code:
    """
      null = <value>
    """
    Then failed with the following message:
    """
    expected null equal to <type>
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
      | ""    | ''      | java.lang.String  |

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
      | ""    | ''      | java.lang.String  |

  Scenario Outline: null matches list
    When assert by the following code:
    """
      null <operator> []
    """
    Then failed with the following message:
    """
    null is not a list
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

  Scenario Outline: null equals to/matches object
    When assert by the following code:
    """
      null <operator> {}
    """
    Then failed with the following message:
    """
    actual value is null
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
    Then the following assertion should pass:
    """
      null <operator> null
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
