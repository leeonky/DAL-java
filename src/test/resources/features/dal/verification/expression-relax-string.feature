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
      ''<opt>,''<opt>
    """
    And the inspect should:
    """
    ''<opt> '' , ''<opt> ''
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: relax string should not be user literal value
    When defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    When evaluate by:
    """
      "$1"= $1
    """
    Then failed with the message:
    """
    Expected to be equal to: com.github.leeonky.dal.compiler.CucumberContextBak$USDollar {
                             ^
        amount: java.lang.Integer <1>
    }
    Actual: java.lang.String
            ^
    <$1>
    """

  Scenario Outline: allowed key word relax string
    * the following verification should pass:
    """
      "<string>"= <string>
    """
    Examples:
      | string |
      | is     |
      | which  |
      | and    |
      | or     |

  Scenario: not allowed key word relax string
    When evaluate by:
    """
      "true"= true
    """
    Then got the following notation:
    """
      "true"= true
              ^
    """
    When evaluate by:
    """
      "false"= false
    """
    Then got the following notation:
    """
      "false"= false
               ^
    """
    When evaluate by:
    """
      "null"= null
    """
    Then got the following notation:
    """
      "null"= null
              ^
    """
