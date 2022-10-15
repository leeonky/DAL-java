Feature: object-scope-relax-string

  Scenario Outline: supported object-scope-relax-string
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
    : {value1: a, value2: b }
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

  Scenario: no verification operand empty string
    Given the following json:
    """
    {
      "value": ""
    }
    """
    Then the following verification should pass:
    """
    : { value:}
    """
    And the inspect should:
    """
    : {value: ''}
    """

  Scenario: relax string should not be user literal value
    When defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    Given the following json:
    """
    {
      "value": "$1"
    }
    """
    When evaluate by:
    """
    value= $1
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
    {
      "value": "<string>"
    }
    """
    * the following verification should pass:
    """
    : { value= <string> }
    """
    Examples:
      | string |
      | is     |
      | which  |
      | and    |
      | or     |

  Scenario Outline: not allowed key word relax string
    Given the following json:
    """
    {
       "value": "<string>"
    }
    """
    When evaluate by:
    """
    : { value= <string> }
    """
    Then got the following notation:
    """
    : { value= <string> }
               ^
    """
    Examples:
      | string |
      | true   |
      | false  |
      | null   |
