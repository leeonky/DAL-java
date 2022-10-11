Feature: node position

  Scenario: should resolve correct position
    Given the following json:
    """
    {
      "string": {
        "key": 1
      },
      "array": [1]
    }
    """
    When evaluate by:
    """
    string: {
      <<"key">>: 2
    }
    """
    Then got the following notation:
    """
    string: {
      <<"key">>: 2
        ^        ^
    }
    """
    When evaluate by:
    """
    array: {
      <<0>>: 2
    }
    """
    Then got the following notation:
    """
    array: {
      <<0>>: 2
        ^    ^
    }
    """
    When evaluate by:
    """
    string: {
      <<["key"]>>: 2
    }
    """
    Then got the following notation:
    """
    string: {
      <<["key"]>>: 2
        ^          ^
    }
    """
