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

  Scenario: raise error when access invalid property
    Given the following java class:
    """
    public class Data {
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
      .invalid
    """
    Then failed with the message:
    """
    Get property `invalid` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `invalid` does not exist in `Data`
    """
    When use a instance of java class "Data" to evaluate:
    """
    null.any
    """
    Then failed with the message:
    """
    Instance is null
    """
    And got the following notation:
    """
    null.any
         ^
    """

  Scenario: raise error when index out of bound
    Given the following json:
    """
      [1, 2]
    """
    When evaluate by:
    """
      [3]
    """
    Then failed with the message:
    """
    Index out of bounds (Index: 3, Size: 2)
    """
    And got the following notation:
    """
      [3]
      ^
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

# TODO invalid element mapping .@ (.@) .@+1 1+.@; valid: a.@b; a@[0]; a.@.b