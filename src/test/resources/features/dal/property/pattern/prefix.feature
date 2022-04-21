Feature: prefix

  Scenario: filter object by property pattern with prefix
    Given the following json:
    """
    {
      "valueA": {
        "value": "A"
      },
      "valueB": {
        "value": "B"
      },
      "valueC": {
        "value": "C"
      }
    }
    """
    When evaluate by:
    """
    value{}
    """
    Then the result should:
    """
    = {
      "a": {
        "value": "A"
      },
      "b": {
        "value": "B"
      },
      "c": {
        "value": "C"
      }
    }
    """
    And the inspect should:
    """
    value{}
    """


#    TODO alias
