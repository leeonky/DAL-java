Feature: verification via regex

  Scenario: verify string by regex
    * the following verification should pass:
    """
    'a'= /a/
    """
    But the following verification should failed:
    """
    'a'= /b/
    """

  Scenario: non string value should use opt ':'
    * the following verification should pass:
    """
    1: /1/
    """
    But the following verification should failed:
    """
    1= /1/
    """
