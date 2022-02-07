Feature: list schema

  Scenario: support element schema expression
    Given the following dal expression:
    """
      [Number / Integer]
    """
    Then parse the following "schema" node:
    """
    : {
      class.simpleName: 'SchemaComposeNode'
      inspect: '[Number / Integer]'
      positionBegin: 2
    }
    """

  Scenario: raise error when no closing ]
    Given the following dal expression:
    """
      [Integer
    """
    Then failed to parse "schema" with the following message:
    """
    should end with `]`
    """
    And got the following notation:
    """
      [Integer
              ^
    """
