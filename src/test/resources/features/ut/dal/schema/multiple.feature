Feature: multiple

  Scenario: multiple schema split by /
    Given the following dal expression:
    """
      a/b / c
    """
    Then parse the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaComposeNode'
      positionBegin: 2
      inspect: 'a / b / c'
    }
    """

  Scenario: raise error when schema list not finished
    Given the following dal expression:
    """
    Integer /
    """
    Then failed to parse "schema" with the following message:
    """
    expect a schema
    """
    And got the following notation:
    """
    Integer /
             ^
    """
