Feature: this

  Scenario: {} as this reference
    Given the following java class:
    """
    public class Data {
      public int value = 10;
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    : {
      {}: {
        value: 10
      }
    }
    """
    And the inspect should:
    """
    : {{}: {value: 10}}
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    = {
      {}: {
        value: 10
      }
    }
    """

  Scenario: {} call static extension method
    Given the following java class:
    """
    public class Data {
      public static int staticMethod(Data data) {
        return 3;
      }
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    : {
      {}: {
        staticMethod: 3
      }
    }
    """

  Scenario: field alias on this reference
    Given the following schema class:
    """
    @Partial
    @FieldAliases({
            @FieldAlias(alias = "aliasOfAge", field = "age"),
    })
    public class User {
    }
    """
    And the following schema class:
    """
    public class Order {
        public User user;
    }
    """
    And the following json:
    """
      {
        "user": {
          "age": 10
        }
      }
    """
    Then the following verification should pass:
    """
      is Order: {
        {}: {
          user: {
            aliasOfAge: 10
          }
        }
      }
    """
