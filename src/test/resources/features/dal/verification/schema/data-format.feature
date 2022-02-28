Feature: build-in data format

  Scenario: should return value in schema 'type'
    When evaluate by:
    """
    "2000-10-10T00:00:00Z" is Instant
    """
    Then the result should:
    """
    class.simpleName: 'Instant'
    """