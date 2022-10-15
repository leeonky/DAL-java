Feature: list-scope-relax-string

  Scenario Outline: supported list-scope-relax-string
    Given the following json:
    """
    [ "a<char>c" ]
    """
    * the following verification should pass:
    """
    = [ a<char>c ]
    """
    And the inspect should:
    """
    = [[0]= 'a<char>c']
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
    [ "a", "b" ]
    """
    Then the following verification should pass:
    """
    : [ a b ]
    """
    And the inspect should:
    """
    : [[0]: 'a', [1]: 'b']
    """
    Then the following verification should pass:
    """
    : [a  b]
    """
    And the inspect should:
    """
    : [[0]: 'a', [1]: 'b']
    """
    Then the following verification should pass:
    """
    : [a
    b]
    """
    And the inspect should:
    """
    : [[0]: 'a', [1]: 'b']
    """
    Then the following verification should pass:
    """
    : [a, b]
    """
    And the inspect should:
    """
    : [[0]: 'a', [1]: 'b']
    """
    Then the following verification should pass:
    """
    : [... b] and [0]: 'a'
    """
    And the inspect should:
    """
    : [..., [-1]: 'b'] and [0]: 'a'
    """

  Scenario: no verification operand empty string
    Given the following json:
    """
    [ "" ]
    """
    Then the following verification should pass:
    """
    : [: ]
    """
    And the inspect should:
    """
    : [[0]: '']
    """

  Scenario: relax string should not be user literal value
    When defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    Given the following json:
    """
    [ "$1" ]
    """
    When evaluate by:
    """
    = [$1]
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
    Given the following json:
    """
    [ "<string>" ]
    """
    * the following verification should pass:
    """
    : [ <string> ]
    """
    Examples:
      | string |
      | which  |
      | and    |
      | or     |

  Scenario Outline: not allowed key word relax string
    Given the following json:
    """
    [ "<string>" ]
    """
    When evaluate by:
    """
    = [ <string> ]
    """
    Then got the following notation:
    """
    = [ <string> ]
        ^
    """
    Examples:
      | string |
      | true   |
      | false  |
      | null   |
