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
