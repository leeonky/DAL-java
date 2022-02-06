Feature: single schema

#  Scenario Outline: raise error when no code
#    Given the following dal expression:
#    """
#    <code>
#    """
#    Then failed to parse "schema" with the following message:
#    """
#    expect a schema
#    """
#    Examples:
#      | code   |
#      | \n     |
#      | `TAB   |
#      | `SPACE |
