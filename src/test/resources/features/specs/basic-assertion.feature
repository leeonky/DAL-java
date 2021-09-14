Feature: basic assertion

  Scenario Outline: non null equal to null
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
    expected [<message>] equal to [null] but was not
    """
    Examples:
      | value | message |
      | 0     | 0       |
      | ""    | ''      |
      | {}    | {}      |
      | []    | []      |

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
    [<message>] does not match null
    """
    Examples:
      | value | message |
      | 0     | 0       |
      | ""    | ''      |
      | {}    | {}      |
      | []    | []      |

  Scenario Outline: null equal to non null
    When assert by the following code:
    """
      null = <value>
    """
    Then failed with the following message:
    """
    expected [null] equal to [<message>] but was not
    """
    And got the following source code information:
    """
      null = <value>
             ^
    """
    Examples:
      | value | message |
      | 0     | 0       |
      | ""    | ''      |
      | {}    | {}      |
      | []    | []      |

  Scenario Outline: null matches non null
    When assert by the following code:
    """
      null : <value>
    """
    Then failed with the following message:
    """
    expected [null] matches [<message>] but was not
    """
    Examples:
      | value | message |
      | 0     | 0       |
      | ""    | ''      |
      | {}    | {}      |
      | []    | []      |

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
      <operand> : *,
      <operand> = *
    """
    Examples:
      | operand |
      | 1       |
      | ''      |
      | null    |
