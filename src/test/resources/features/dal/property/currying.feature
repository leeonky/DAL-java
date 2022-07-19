Feature: currying function

  Scenario: support currying instance method with 1 arg
    Given the following java class:
    """
    public class Data {
      public String upperCase(String input) {
        return input.toUpperCase();
      }
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    upperCase: {
      a: A
      b: B
      c: C
    }
    """

# TODO 1 level, 2 level
# TODO args range for = {} verification
# TODO Auto convert sub property to function args (property is string)
# TODO error handling when curryingmethod not support sub args rang in object= / filter property /