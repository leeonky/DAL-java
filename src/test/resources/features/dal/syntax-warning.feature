Feature: syntax-warning

  Scenario: give an warning when have space between property in object verification
    Given the following json:
    """
    {
      "length": 5,
      "value": "value"
    }
    """
    Then the following verification should pass:
    """
    : {
      length= 'value'
      [length]
    }
    """
    But got the following warning:
    """
    : {
      length= 'value'
                    ^
      [length]
      ^
    }

    Warning: Ambiguity detected. Please add a comma or remove whitespace to clear this warning.
    """
    Then the following verification should pass:
    """
    : {
      length= .value
      [length]
    }
    """
    But got the following warning:
    """
    : {
      length= .value
                   ^
      [length]
      ^
    }

    Warning: Ambiguity detected. Please add a comma or remove whitespace to clear this warning.
    """
    Then the following verification should pass:
    """
    : {
      length= .value
      .length
    }
    """
    But got the following warning:
    """
    : {
      length= .value
                   ^
      .length
      ^
    }

    Warning: Ambiguity detected. Please add a comma or remove whitespace to clear this warning.
    """

# space before . [] :: in any expression