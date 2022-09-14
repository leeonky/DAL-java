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
    And the following verification for the instance of java class "Data" should pass:
    """
    upperCase: {
      1= '1'
    }
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

  Scenario: should not use instance static method in currying
    Given the following java class:
    """
    public class Data {
      public static String upperCase(String input) {
        return input.toUpperCase();
      }
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    upperCase[1]= '1'
    """
    Then failed with the message:
    """
    Get property `upperCase` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `upperCase` does not exist in `#package#Data`
    """

  Scenario: should use same arg type of method in instance method currying
    Given the following java class:
    """
    public class Data {
      public String method(String input) {
        return "string";
      }
      public String method(CharSequence chars) {
        return "chars";
      }
      public String method(int input) {
        return "int";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method.hello= string
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method[1]= int
    """

  Scenario: should use base arg type of method in instance method currying
    Given the following java class:
    """
    public class Data {
      public String method(CharSequence chars) {
        return "chars";
      }
      public String method(int input) {
        return "int";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method['100']= chars
    """

  Scenario: should use convertible arg type of method in instance method currying
    Given the following java class:
    """
    public class Data {
      public String method(String str) {
        return "string";
      }
      public String method(java.io.File file) {
        return "file";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method[100]= string
    """

  Scenario: should use same arg type of method in static method currying
    Given the following java class:
    """
    public class Data {
      public static String method(Data data, String input) {
        return "string";
      }
      public static String method(Data data, CharSequence chars) {
        return "chars";
      }
      public static String method(Data data, int input) {
        return "int";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method.hello= string
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method[1]= int
    """

  Scenario: should use base arg type of method in static method currying
    Given the following java class:
    """
    public class Data {
      public static String method(Data data, CharSequence chars) {
        return "chars";
      }
      public static String method(Data data, int input) {
        return "int";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method['100']= chars
    """

  Scenario: should use convertible arg type of method in static method currying
    Given the following java class:
    """
    public class Data {
      public static String method(Data data, String str) {
        return "string";
      }
      public static String method(Data data, java.io.File file) {
        return "file";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method[100]= string
    """

  Scenario: raise error when more than one base arg type of method in instance method currying
    Given the following java class:
    """
    public class Data {
      public String method(CharSequence chars) {
        return "chars";
      }
      public String method(Object input) {
        return "object";
      }
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    method.str
    """
    Then failed with the message:
    """
    Get property `str` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    More than one currying method:
      public java.lang.String #package#Data.method(java.lang.CharSequence)
      public java.lang.String #package#Data.method(java.lang.Object)
    """
    And got the following notation:
    """
    method.str
           ^
    """

  Scenario: should use same instance type of method in static method currying
    Given the following java class:
    """
    public class BaseData {
    }
    """
    Given the following java class:
    """
    public class Data extends BaseData {
      public static String method(BaseData data, String input) {
        return "super";
      }
      public static String method(Data data, String input) {
        return "sub";
      }
    }
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    method.hello= sub
    """
