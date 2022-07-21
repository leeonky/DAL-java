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

  Scenario: support currying instance method with 2 arg
    Given the following java class:
    """
    public class Data {
      public String upperCase(String input, String arg2) {
        return input.toUpperCase() + arg2;
      }
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    upperCase: {
      a.b: Ab
      c: {
        x: Cx
        y: Cy
      }
    }
    """

  Scenario: support args range
    Given the following java class:
    """
    public class Data {
      public String upperCase(String input) {
        return input.toUpperCase();
      }
    }
    """
    And args range of java class "Data" method "upperCase":
      | a | b | c |
    When use a instance of java class "Data" to evaluate:
    """
    upperCase= {
      a: A
    }
    """
    Then failed with the message:
    """
    Unexpected fields `b`, `c` in upperCase
    """
    And got the following notation:
    """
    upperCase= {
             ^
      a: A
    }
    """

  Scenario: convent arg to proper type
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
    upperCase[1]= '1'
    """

  Scenario: static method currying
    Given the following java class:
    """
    public class Data {
    }
    """
    And the following java class:
    """
    public class DataMethods {
      public static String property(Data data, String str) {
        return str.toUpperCase();
      }
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    property: {
      a: A
    }
    """

# TODO input is null
