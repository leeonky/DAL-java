Feature: chain

  Scenario: return root object when no code
    Given the following json:
    """
    1
    """
    When evaluate by:
    """
    """
    Then the result should:
    """
    : 1
    """
    And the inspect should:
    """
    """

  Scenario Outline: chain after arithmetic
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code                   | value | inspect                |
      | 1+ -2 *3               | -5    | 1 + -2 * 3             |
      | 1+1: 2                 | 2     | 1 + 1: 2               |
      | 'a' + 'b' . length     | 'a1'  | 'a' + 'b'.length       |
      | 'a' + 'b' which length | 2     | 'a' + 'b' which length |
      | 4/2 is Number          | 2     | 4 / 2 is Number        |

  Scenario Outline: chain after verification
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code                | value | inspect             |
      | 4: 2+2              | 4     | 4: 2 + 2            |
      | 4: 4: 4             | 4     | 4: 4: 4             |
      | '4': '4'.toString   | '4'   | '4': '4'.toString   |
      | 4: 4 which toString | '4'   | 4: 4 which toString |
      | 4: 4 is Number      | 4     | 4: 4 is Number      |

  Scenario Outline: chain after property
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code                      | value | inspect                   |
      | 'a'.length+2              | 3     | 'a'.length + 2            |
      | 'a'.length: 1             | 1     | 'a'.length: 1             |
      | 'a'.length.toString       | '1'   | 'a'.length.toString       |
      | 'a'.length which toString | '1'   | 'a'.length which toString |
      | 'a'.length is Number      | 1     | 'a'.length is Number      |

  Scenario Outline: chain after which
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code                            | value | inspect                         |
      | 'a' which length+1              | 2     | 'a' which length + 1            |
      | 'a' which length:1              | 1     | 'a' which length: 1             |
      | 'a' which length.toString       | '1'   | 'a' which length.toString       |
      | 'a' which length which toString | '1'   | 'a' which length which toString |
      | 'a' which length is Number      | 1     | 'a' which length is Number      |

  Scenario Outline: chain after schema
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code                  | value | inspect               |
      | 1 is Number :1        | 1     | 1 is Number: 1        |
      | 1 is Number which 1   | 1     | 1 is Number which 1   |
      | 1 is Number is Number | 1     | 1 is Number is Number |
