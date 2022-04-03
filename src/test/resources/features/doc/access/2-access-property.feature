Feature: access data

  Scenario: return input value when no code
    Given the following json:
    """
    1
    """
    When evaluate by:
    """
    """
    Then the result should:
    """
    = 1
    """

  Scenario: property chain
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }]
      }
    """
    When evaluate by:
    """
      items[0].id
    """
    Then the result should:
    """
    : 100
    """

  Scenario: cann access public field, public getter, public no arg method by property
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
    But the following verification should failed:
    """
    .privateProperty
    """
    But the following verification should failed:
    """
    .nonStaticProperty
    """

  Scenario: evaluate all as a list
    When evaluate all by:
    """
      1 2 3
    """
    Then the result should:
    """
    : [1 2 3]
    """