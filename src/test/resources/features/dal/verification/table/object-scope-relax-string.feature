Feature: object-scope-relax-string

  Scenario Outline: supported expression-relax-string
    Given the following json:
    """
    {
      "value": "a<char>c"
    }
    """
    * the following verification should pass:
    """
    : { value= a<char>c }
    """
    And the inspect should:
    """
    : {value= 'a<char>c'}
    """
    Examples:
      | char |
      | b    |
      | +    |
      | -    |
      | *    |
      | /    |
      | .    |
      | :    |
      | #    |
      | //   |
      | /*   |

  Scenario: split relax string
    Given the following json:
    """
    {
      "value1": "a",
      "value2": "b"
    }
    """
    Then the following verification should pass:
    """
    : { value1: a value2: b }
    """
    And the inspect should:
    """
    : {value1: 'a', value2: 'b'}
    """
    Then the following verification should pass:
    """
    : {value1: a  value2: b }
    """
    And the inspect should:
    """
    : {value1: 'a', value2: 'b'}
    """
    Then the following verification should pass:
    """
    : { value1: a
    value2: b }
    """
    And the inspect should:
    """
    : {value1: 'a', value2: 'b'}
    """
    Then the following verification should pass:
    """
    : { value1: a} and value2: b
    """
    And the inspect should:
    """
    : {value1: 'a'} and value2: 'b'
    """

#   TODO not user defined literal
#   TODO not key word
