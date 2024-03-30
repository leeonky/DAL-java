Feature: syntax-warning

  Scenario: give an warning when have space between property in object verification
    Given the following json:
    """
    {
      "length": 5,
      "value": "value",
      "list": [1, 2, 3, 4, 5]
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

    Warning: may be ambiguous. Please add a comma or remove whitespace to clear this warning.
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

    Warning: may be ambiguous. Please add a comma or remove whitespace to clear this warning.
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

    Warning: may be ambiguous. Please add a comma or remove whitespace to clear this warning.
    """
    Then the following verification should pass:
    """
    : {
      length= .list
      ::size
    }
    """
    But got the following warning:
    """
    : {
      length= .list
                  ^
      ::size
      ^
    }

    Warning: may be ambiguous. Please add a comma or remove whitespace to clear this warning.
    """
