Feature: support line comment

  Scenario Outline: first line start with comment char
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment>
    = 1
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario Outline: multi comment lines
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment> comment 1
    <comment> comment 2
    = 1
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario Outline: blank line between comment lines
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment> comment 1

    <comment> comment 2
    = 1
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario Outline: only comments with no code
    When the following json:
    """
    1
    """
    Then the following verification should pass:
    """
    <comment>
    """
    Examples:
      | comment |
      | #       |
      | //      |

  Scenario: comments in code
    When the following json:
    """
    {
      "key1": "value1",
      "key2": "value2"
    }
    """
    Then the following verification should pass:
    """
    = {
#    this is a comment
      key1: "value1" # comment after clause

//    this is anther comment

      key2: "value2"
    }
    """
    When evaluate by:
    """
    = {
#    this is a comment
      key1: "value1" # comment after clause

//    this is anther comment

      key2: "value3"
    }
    """
    Then got the following notation:
    """
    = {
#    this is a comment
      key1: "value1" # comment after clause

//    this is anther comment

      key2: "value3"
            ^
    }
    """

  Scenario: comments bug
    When the following json:
    """
    {
      "key1": "value1",
      "key2": "value2"
    }
    """
    Then the following verification should pass:
    """
    = {
      key1: "value1"
      key2: "value2" # comment 1
//    comment 2
    } # comment 3
//    comment 4
    """

  Scenario: comments in table
    When the following json:
    """
    [{
      "key1": "value1",
      "key2": "value2"
    },{
      "key1": "value3",
      "key2": "value4"
    }]
    """
    Then the following verification should pass:
    """
    = | key1   | key2   |
      | 'value1' | 'value2' |
#      | 'value' | 'value' |
      | 'value3' | 'value4' |
    """