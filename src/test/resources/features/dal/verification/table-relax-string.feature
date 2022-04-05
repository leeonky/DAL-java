Feature: table-relax-string

  Scenario Outline: supported table-relax-string
    Given the following json:
    """
    [{
      "value": "a<char>c"
    }]
    """
    * the following verification should pass:
    """
    = | value    |
      | a<char>c |
    """
    And the inspect should:
    """
    = | value |
    | value= 'a<char>c' |
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

  Scenario: numbers in table-relax-string
    Given the following json:
    """
    [{
      "value": "1 2"
    }]
    """
    * the following verification should pass:
    """
    = | value |
      | 1 2   |
    """
    And the inspect should:
    """
    = | value |
    | value= '1 2' |
    """

  Scenario: split relax string
    Given the following json:
    """
    [{
      "value1": "a",
      "value2": "b"
    }]
    """
    * the following verification should pass:
    """
    = | value1 | value2 |
      | a| b |
    """

  Scenario: no verification operand empty string
    Given the following json:
    """
    [{
      "value1": ""
    }]
    """
    Then the following verification should pass:
    """
    : | value1 |
      |        |
    """
    And the inspect should:
    """
    : | value1 |
    | value1: '' |
    """

  Scenario: relax string should not be user literal value
    When defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    Given the following json:
    """
    [{
      "value": "$1"
    }]
    """
    When evaluate by:
    """
    = | value |
      | $1    |
    """
    Then failed with the message:
    """
    Expecting java.lang.String
    <$1>
    to be equal to com.github.leeonky.dal.compiler.CucumberContextBak$USDollar
    <1$>
    but was not
    """

  Scenario Outline: allowed key word relax string
    Given the following json:
    """
    [{
      "value": "<string>"
    }]
    """
    * the following verification should pass:
    """
    : | value |
      | <string> |
    """
    Examples:
      | string |
      | which  |
      | and    |
      | or     |

  Scenario Outline: not allowed key word relax string
    Given the following json:
    """
    [{
      "value": "<string>"
    }]
    """
    When evaluate by:
    """
    = | value |
      | <string> |
    """
    Then got the following notation:
    """
    = | value |
      | <string> |
        ^
    <notation>
    """
    Examples:
      | string | notation    |
      | true   | ^^^^^^^^^^  |
      | false  | ^^^^^^^^^^^ |
      | null   | ^^^^^^^^^^  |
