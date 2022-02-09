Feature: static method

  Scenario: public static method extension
    Given the following java class:
    """
    public class Data {
    }
    """
    And the following java class:
    """
    public class DataMethods {
      public static int property(Data data) {
        return 1;
      }
      private static int privateProperty(Data data) {
        return 2;
      }
      public int nonStaticProperty(Data data) {
        return 3;
      }
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    property: 1
    """
    When evaluate by:
    """
    privateProperty
    """
    Then failed with the message:
    """
    Get property `privateProperty` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `privateProperty` does not exist in `Data`
    """
    And got the following notation:
    """
    privateProperty
    ^
    """
    When evaluate by:
    """
    nonStaticProperty
    """
    Then failed with the message:
    """
    Get property `nonStaticProperty` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `nonStaticProperty` does not exist in `Data`
    """
    And got the following notation:
    """
    nonStaticProperty
    ^
    """
