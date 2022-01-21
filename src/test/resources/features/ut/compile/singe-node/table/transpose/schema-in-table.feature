Feature: schema in table

  Scenario: compile schema in row
    Given the following dal code:
    """
    | >>   | is Person |        |
    | name | 'Tom'     | 'John' |
    """
    Then got the following "table" node:
    """
    inspect: "| >> | is Person |  |
    | name | : 'Tom' | : 'John' |"
    """

  Scenario: compile schema in header
    Given the following dal code:
    """
    >>| name is String | 'Tom' |
    """
    Then got the following "table" node:
    """
    inspect: ">>| name is String | is String: 'Tom' |"
    """

  Scenario: compile schema in cell
    Given the following dal code:
    """
    >>| name | is String |
    """
    Then got the following "table" node:
    """
    inspect: ">>| name | is String |"
    """

  Scenario: compile schema in header and cell
    Given the following dal code:
    """
    >>| time is String | is Instant: {year: 2000} |
    """
    Then got the following "table" node:
    """
    : {
      inspect: ">>| time is String | is Instant: {year: 2000} |"
      rows.cells: [[{
        class.simpleName: 'DALExpression'
        operator.class.simpleName: 'Matcher'
        leftOperand: {
          class.simpleName: 'SchemaExpression'
          inspect: 'time is String is Instant'
        }
        rightOperand: {
          class.simpleName: 'ObjectNode'
          inspect: "{year: 2000}"
        }
      }]]
    }
    """

  Scenario: judgement by element schema of table
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero {
        public int id = 0;
    }
    """
    Given the following input data:
    """
    [{
      "id": 1,
      "name": 'Tom'
    }]
    """
    When assert by the following code:
    """
    is [IdZero]: >>| aliasOfName | 'Tom' |
    """
    Then failed with the following message:
    """
    Expecting [0] to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following source code information:
    """
    is [IdZero]: >>| aliasOfName | 'Tom' |
        ^
    """
    Given the following input data:
    """
    [{
      "id": 0,
      "name": 'Tom'
    }]
    """
    Then the following assertion should pass:
    """
    is [IdZero]: >>| aliasOfName | 'Tom' |
    """

  Scenario: judgement by schema in row
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero {
        public int id = 0;
    }
    """
    And the following input data:
    """
      [{
        "id": 1,
        "name": 'Tom'
      }]
    """
    When assert by the following code:
    """
    : | >>   | is IdZero |
      | name | 'Tom'     |
    """
    Then failed with the following message:
    """
    Expecting [0] to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following source code information:
    """
    : | >>   | is IdZero |
                  ^
      | name | 'Tom'     |
               ^
    """
    When the following input data:
    """
      [{
        "id": 0,
        "name": 'Tom'
      }]
    """
    Then the following assertion should pass:
    """
    : | >>          | is IdZero |
      | aliasOfName | 'Tom'     |
    """

  Scenario: judgement by schema in header
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero {
        public int id = 0;
    }
    """
    Given the following input data:
    """
    [{
      "obj": {
        "id": 1,
        "name": 'Tom'
      }
    }]
    """
    When assert by the following code:
    """
    : >>| obj is IdZero | {aliasOfName: 'Tom'} |
    """
    Then failed with the following message:
    """
    Expecting obj to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following source code information:
    """
    : >>| obj is IdZero | {aliasOfName: 'Tom'} |
                 ^
                          ^
    """
    When the following input data:
    """
    [{
      "obj": {
        "id": 0,
        "name": 'Tom'
      }
    }]
    """
    Then the following assertion should pass:
    """
    : >>| obj is IdZero | {aliasOfName: 'Tom'} |
    """

  Scenario: judgement by schema in cell
    Given the following schema:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfName", field = "name")
    })
    public class IdZero {
        public int id = 0;
    }
    """
    Given the following input data:
    """
    [{
      "obj": {
        "id": 1,
        "name": 'Tom'
      }
    }]
    """
    When assert by the following code:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
    """
    Then failed with the following message:
    """
    Expecting obj to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following source code information:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
                ^
                   ^
    """
    When the following input data:
    """
    [{
      "obj": {
        "id": 0,
        "name": 'Tom'
      }
    }]
    """
    Then the following assertion should pass:
    """
    : >>| obj | is IdZero: {aliasOfName: 'Tom'} |
    """
