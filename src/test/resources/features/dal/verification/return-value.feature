Feature: return value

  Scenario: should return actual value when verification passed
    Given the following json:
    """
    {
      "i": 100,
      "str": "hello"
    }
    """
    Then the following verification should pass:
    """
    i= 100 = 100
    """
    Then the following verification should pass:
    """
    str= hello = hello
    """

  Scenario: should return actual value(do not convert) when verification passed
    Given the following json:
    """
    {
      "d": 100.0
    }
    """
    Then the following verification should pass:
    """
    (d: 100) = 100.0
    """

  Scenario: should return actual value in object verification
    Given the following json:
    """
    {
      "d": 100.0
    }
    """
    Then the following verification should pass:
    """
    = {
      d: 100.0
    }
    = {
      d: 100.0
    }
    """

  Scenario: should return actual value in list verification
    Given the following json:
    """
    [1, 2, 3, 4]
    """
    Then the following verification should pass:
    """
    = [1, 2, 3, 4]
    = [1, 2, 3, 4]
    """
