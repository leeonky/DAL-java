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
    [1, 2, "3", null]
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
    [[1], [1, 2], []]
    """
    Then dumped data should be:
    """
    [[1], [1, 2], []]
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
        "name": "Tom"
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
        "name": "Tom"
      },
      "c": {}
    }
    """

  Scenario: list of object
    Given the following json:
    """
    [
      {
        "name": "John"
      },
      {
         "name": "Tom"
      },
      {},
      {
        "name": "Jerry"
      }
    ]
    """
    Then dumped data should be:
    """
    [{
      "name": "John"
    }, {
      "name": "Tom"
    }, {}, {
      "name": "Jerry"
    }]
    """

  Scenario: circle reference
    Given the following java class:
    """
    public class Data {
      public int value = 1;
      public Data getThis() {
        return this;
      }
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    {
      "value": 1,
      "this": "** same with root"
    }
    """

  Scenario: circle reference of sub object
    Given the following java class:
    """
    public class Data {
      public int value = 1;
      public SubData subData = new SubData();
    }
    """
    And the following java class:
    """
    public class SubData {
      public SubData getThis() {
        return this;
      }
      public int value = 2;
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    {
      "value": 1,
      "subData": {
        "value": 2,
        "this": "** same with .subData"
      }
    }
    """

  Scenario: should not be circle reference for empty map
    Given the following json:
    """
    { "a": {}, "b": {}, "c": {} }
    """
    Then dumped data should be:
    """
    {
      "a": {},
      "b": {},
      "c": {}
    }
    """

  Scenario: same reference of list
    Given the following json:
    """
    [[{ "a": 1 }], [{ "a": 1 }]]
    """
    Then dumped data should be:
    """
    [[{
      "a": 1
    }], "** same with [0]"]
    """

  Scenario: ignore empty list in list reference
    Given the following json:
    """
    [[], [], []]
    """
    Then dumped data should be:
    """
    [[], [], []]
    """

  Scenario: complex data dump
    Given the following json:
    """
    [{
      "obj": [{
        "user": {
          "name": "John"
         }
      }]
    }, {
      "name": "Tom"
    }, {
       "name": "John"
    }]
    """
    Then dumped data should be:
    """
    [{
      "obj": [{
        "user": {
          "name": "John"
        }
      }]
    }, {
      "name": "Tom"
    }, "** same with [0].obj[0].user"]
    """

# TODO value type
