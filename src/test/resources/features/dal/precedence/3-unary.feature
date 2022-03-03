Feature: unary

  Scenario Outline: minus precedence
    When evaluate by:
    """
      <input> <operator> -<value>
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <input> <operator> -<value>
    """
    Examples:
      | input | operator | value | result |
      | 10    | +        | 1     | 9      |
      | 10    | -        | 1     | 11     |
      | 10    | *        | 2     | -20    |
      | 10    | /        | 2     | -5     |
      | 2     | >        | 5     | true   |
      | 2     | <        | 4     | false  |
      | 2     | >=       | 5     | true   |
      | 2     | <=       | 5     | false  |
      | 3     | !=       | 3     | true   |

  Scenario Outline: logic not precedence
    When evaluate by:
    """
      <input> <operator> !<value>
    """
    Then the result should:
    """
    : <result>
    """
    And the inspect should:
    """
    <input> <operator> !<value>
    """
    Examples:
      | input | operator | value | result |
      | true  | &&       | true  | false  |
      | false | \|\|     | false | true   |
      | true  | and      | true  | false  |
      | false | or       | true  | false  |

  Scenario Outline: with which
    When evaluate by:
    """
    which <input>
    """
    Then the result should:
    """
    : <input>
    """
    Examples:
      | input |
      | -1    |
      | !true |

  Scenario: with opt is
    * the following verification should syntax error:
    """
    is -1
    """

  Scenario Outline: minus with verification
    Given the following json:
    """
    -1
    """
    When the following verification should pass:
    """
    <opt> -<result>
    """
    Examples:
      | opt | result |
      | :   | 1      |
      | =   | 1      |

  Scenario Outline: logic not with verification
    Given the following json:
    """
    false
    """
    When the following verification should pass:
    """
    <opt> !<result>
    """
    Examples:
      | opt | result |
      | :   | true   |
      | =   | true   |
