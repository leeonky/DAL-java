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

  Scenario: dump object
    Given the following json:
    """
    {
      "name": "John"
    }
    """
    Then dumped data should be:
    """
    {
      "name": "John"
    }
    """

  Scenario: empty object
    Given the following json:
    """
    { }
    """
    Then dumped data should be:
    """
    {}
    """

  Scenario: nested object
    Given the following json:
    """
    {
      "a": {
        "name": "John"
      },
      "b": {
        "name": "John"
      },
      "c": {}
    }
    """
    Then dumped data should be:
    """
    {
      "a": {
        "name": "John"
      },
      "b": {
        "name": "John"
      },
      "c": {}
    }
    """
