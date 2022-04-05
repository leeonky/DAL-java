Feature: expression-relax-string

  Scenario Outline: supported expression-relax-string
    * the following verification should pass:
    """
      'a<char>c'= a<char>c
    """
    And the inspect should:
    """
    'a<char>c'= 'a<char>c'
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
    * the following verification should pass:
    """
      'abc'= abc and 'def'= def  # space
    """
    And the inspect should:
    """
    'abc'= 'abc' and 'def'= 'def'
    """
    * the following verification should pass:
    """
      'abc'= abc
    and 'def'= def
    # \n
    """
    And the inspect should:
    """
    'abc'= 'abc' and 'def'= 'def'
    """
    * the following verification should pass:
    """
      'abc'= abc  and 'def'= def  # \t
    """
    And the inspect should:
    """
    'abc'= 'abc' and 'def'= 'def'
    """
    * the following verification should pass:
    """
      'abc'= abc,'def'= def  # \t
    """
    And the inspect should:
    """
    'abc'= 'abc' , 'def'= 'def'
    """
    * the following verification should pass:
    """
      'abc'= abc||'def'= def
    """
    And the inspect should:
    """
    'abc'= 'abc' || 'def'= 'def'
    """
    * the following verification should pass:
    """
      'abc'= abc&&'def'= def
    """
    And the inspect should:
    """
    'abc'= 'abc' && 'def'= 'def'
    """

  Scenario Outline: no verification operand empty string
    * the following verification should pass:
    """
      ''<opt>
    """
    And the inspect should:
    """
    ''<opt> ''
    """
    Examples:
      | opt |
      | =   |
      | :   |

#  Scenario: simple string in table
#    Given the following json:
#    """
#    [{
#      "name": "Tom"
#    }]
#    """
#    Then the following verification should pass:
#    """
#    : | name |
#      | Tom  |
#    """
#    And the inspect should:
#    """
#    : | name |
#    | name: 'Tom' |
#    """

#   TODO not user defined literal
#   TODO not key word
