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

#  TODO this field alias;
