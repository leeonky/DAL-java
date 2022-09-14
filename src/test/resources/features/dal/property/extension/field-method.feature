Feature: field-method

  Scenario: can access public field, public getter, public no arg method by property
    Given the following java class:
    """
    public class Data {
      public int publicField = 100;

      public int getGetter() {
        return 200;
      }
      public int noArgMethod() {
        return 300;
      }

      private int privateField = 200;
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    publicField: 100
    """
    When evaluate by:
    """
    getter
    """
    Then the result should:
    """
    : 200
    """
    When evaluate by:
    """
    noArgMethod
    """
    Then the result should:
    """
    : 300
    """
    When evaluate by:
    """
    privateField
    """
    Then failed with the message:
    """
    Get property `privateField` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `privateField` does not exist in `#package#Data`
    """
