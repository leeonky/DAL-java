Feature: dump-data

  Scenario Outline: dump single value
    Given the following json:
    """
    <value>
    """
    Then dumped data should be:
    """
    <expected>
    """
    Examples:
      | value   | expected |
      | null    | null     |
      | 100.1   | 100.1    |
      | "hello" | "hello"  |

  Scenario: dump list
    Given the following json:
    """
    [ 1, 2, "3", null]
    """
    Then dumped data should be:
    """
    [
      1,
      2,
      "3",
      null
    ]
    """

  Scenario: dump empty list
    Given the following json:
    """
    []
    """
    Then dumped data should be:
    """
    []
    """

  Scenario: dump nested list
    Given the following json:
    """
    [ [1], [1, 2], [] ]
    """
    Then dumped data should be:
    """
    [
      [
        1
      ],
      [
        1,
        2
      ],
      []
    ]
    """
