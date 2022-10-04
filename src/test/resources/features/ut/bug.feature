Feature: bug

  Scenario: start with notation error when start withs key words 1
    Given the following json:
    """
    {
      "is": "str_is",
      "order": "str_order",
      "iso": "str_iso",
      "whichA": "which_str"
    }
    """
    Then the following verification should pass:
    """
    = {
      iso= str_iso
      'is'= str_is
      order= str_order
      whichA= which_str
    }
    """

  Scenario: start with notation error when start withs key words 2
    Given the following json:
    """
    {
      "a": true,
      "andB": false,
      "B": false
    }
    """
    Then the following verification should pass:
    """
    : {
      a= true andB= false
    }
    """

