Feature: verify object

  Scenario: table one header and one row
    Given the following json:
    """
    {
      "row0": {
        "col": 1
      },
      "row1": {
        "col": 2
      }
    }
    """
    Then the following verification should pass:
    """
    :    | col |
    row0 | 1   |
    row1 | 2   |
    """
    And the inspect should:
    """
    : | col |
    row0 | col: 1 |
    row1 | col: 2 |
    """

  Scenario: invalid property key table
    Given the following json:
    """
    {
      "row0": {
        "col": 1
      },
      "row1": {
        "col": 2
      }
    }
    """
    When evaluate by:
    """
    :    | col |
    row0 | 1   |
         | 2   |
    """
    Then failed with the message:
    """
    Row index should be consistent
    """
    And got the following notation:
    """
    :    | col |
    row0 | 1   |
    ^^^^^^^^^^^^
         | 2   |
    ^^^^^^^^^^^^
    """

  Scenario: verify all keys in object
    Given the following json:
    """
    {
      "row0": {
        "col": 1
      },
      "row1": {
        "col": 2
      }
    }
    """
    When evaluate by:
    """
     =    | col |
     row0 | 1   |
    """
    Then failed with the message:
    """
    Unexpected fields `row1`
    """
    And got the following notation:
    """
     =    | col |
     ^
     row0 | 1   |
    """

  Scenario: empty table for object
    Given the following json:
    """
    {}
    """
    Then the following verification should pass:
    """
     =    | col |
    """

  Scenario: transposed table one header and one row
    Given the following json:
    """
    {
      "row0": {
        "col": 1
      },
      "row1": {
        "col": 2
      }
    }
    """
    Then the following verification should pass:
    """
    : | >>  | row0 |
      | col | 1    |
    """
    And the inspect should:
    """
    : | >> | row0 |
    | col | col: 1 |
    """

  Scenario: number is row key
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, Object> data = new java.util.HashMap<Object, Object>() {{
        put(0, new java.util.HashMap<Object, String>() {{
          put("a", "0a");
          put("b", "0b");
        }});
        put(1, new java.util.HashMap<Object, String>() {{
          put("a", "1a");
          put("b", "1b");
        }});
      }};
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data:  | a    | b    |
         0 | '0a' | '0b' |
         1 | '1a' | '1b' |
    """
    And the inspect should:
    """
    data: | a | b |
    0 | a: '0a' | b: '0b' |
    1 | a: '1a' | b: '1b' |
    """
