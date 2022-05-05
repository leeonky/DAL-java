Feature: dump-data

  Scenario Outline: dump single value
    Given the following json:
    """
    <value>
    """
    Then dumped data should be:
    """
    : <expected>
    """
    Examples:
      | value   | expected |
      | null    | null     |
      | 100.1   | 100.1    |
      | "hello" | "hello"  |
