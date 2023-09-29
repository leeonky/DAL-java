Feature: schema in table

  Scenario: verification with element schema of table
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    Given the following json:
    """
    [{
      "id": 1,
      "name": "Tom"
    }]
    """
    When evaluate by:
    """
    is [IdZero]: >>| aliasOfName | 'Tom' |
    """
    Then failed with the message:
    """
    Expected [0] to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.Integer
        <1>
    """
    And got the following notation:
    """
    is [IdZero]: >>| aliasOfName | 'Tom' |
        ^
    """
    Given the following json:
    """
    [{
      "id": 0,
      "name": "Tom"
    }]
    """
    Then the following verification should pass:
    """
    is [IdZero]: >>| aliasOfName | 'Tom' |
    """
    And the inspect should:
    """
    is [IdZero]: >>| aliasOfName | : 'Tom' |
    """

  Scenario: use schema in row
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    And the following json:
    """
      [{
        "id": 1,
        "name": "Tom"
      }]
    """
    When evaluate by:
    """
    : | >>   | is IdZero |
      | name | 'Tom'     |
    """
    Then failed with the message:
    """
    Expected [0] to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.Integer
        <1>
    """
    And got the following notation:
    """
    : | >>   | is IdZero |
                  ^
               ^
      | name | 'Tom'     |
               ^
    """
    When the following json:
    """
      [{
        "id": 0,
        "name": "Tom"
      }]
    """
    Then the following verification should pass:
    """
    : | >>          | is IdZero |
      | aliasOfName | 'Tom'     |
    """
    And the inspect should:
    """
    : | >> | is IdZero |
    | aliasOfName | : 'Tom' |
    """

  Scenario: use schema in header
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    Given the following json:
    """
    [{
      "obj": {
        "id": 1,
        "name": "Tom"
      }
    }]
    """
    When evaluate by:
    """
    : >>| obj is IdZero | {aliasOfName: 'Tom'} |
    """
    Then failed with the message:
    """
    Expected obj to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.Integer
        <1>
    """
    And got the following notation:
    """
    : >>| obj is IdZero | {aliasOfName: 'Tom'} |
                 ^
                          ^
    """
    When the following json:
    """
    [{
      "obj": {
        "id": 0,
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : >>| obj is IdZero | {aliasOfName: 'Tom'} |
    """
    And the inspect should:
    """
    : >>| obj is IdZero | : {aliasOfName: 'Tom'} |
    """

  Scenario: use schema in cell
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero implements Schema {
        public int id = 0;
    }
    """
    Given the following json:
    """
    [{
      "obj": {
        "id": 1,
        "name": "Tom"
      }
    }]
    """
    When evaluate by:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
    """
    Then failed with the message:
    """
    Expected obj to match schema `IdZero` but was not
        Expected field `.id` to be java.lang.Integer
        <0>
        Actual: java.lang.Integer
        <1>
    """
    And got the following notation:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
                   ^
                ^
    """
    When the following json:
    """
    [{
      "obj": {
        "id": 0,
        "name": "Tom"
      }
    }]
    """
    Then the following verification should pass:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
    """
    And the inspect should:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
    """

  Scenario: use schema in header and cell
    When the following json:
    """
    [{
      "time": "2000-10-10T00:00:00"
    }]
    """
    Then the following verification should pass:
    """
    : >>| time is String | is LocalDateTime: {year: 2000} |
    """
    And the inspect should:
    """
    : >>| time is String | is LocalDateTime: {year: 2000} |
    """
