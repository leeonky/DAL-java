Feature: list mapping

  Scenario: Map list element
    Given the following json:
    """
      {
        "items": [{
          "id": 100
        }, {
          "id": 200
        }]
      }
    """
    When evaluate by:
    """
      items.id[]
    """
    Then the result should:
    """
    : [100 200]
    """
    And the inspect should:
    """
    items.id[]
    """

  Scenario: List mapping chain
    Given the following json:
    """
      {
        "products": [{
          "catalog": {
            "sub": {
              "value": {
                "string": "001"
              }
            }
          }
        }, {
          "catalog": {
            "sub": {
              "value": {
                "string": "002"
              }
            }
          }
        }]
      }
    """
    When evaluate by:
    """
      products.catalog[].sub.value.string
    """
    Then the result should:
    """
    : ["001" "002"]
    """
    And the inspect should:
    """
    products.catalog[].sub.value.string
    """

  Scenario: disable implicit list mapping
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
    items.id= [100]
    """
    Then failed with the message:
    """
    Get property `id` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `id` does not exist in `java.util.ArrayList`
    Implicit list mapping is not allowed in current version of DAL, use `id[]` instead
    """
    And got the following notation:
    """
    items.id= [100]
          ^
    """

  Scenario: raise error when access invalid property
    Given the following java class:
    """
    public class Data {
      public Object[] list = {new Object()};
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
      list.invalid[]
    """
    Then failed with the message:
    """
    Mapping element[0]:
    Get property `invalid` failed, property can be:
      1. public field
      2. public getter
      3. public no args method
      4. Map key value
      5. customized type getter
      6. static method extension
    Method or property `invalid` does not exist in `java.lang.Object`
    """
    And got the following notation:
    """
      list.invalid[]
           ^
    """

  Scenario: raise error when instance is not list
    Given the following java class:
    """
    public class Data {
      public Object list = new Object();
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
      list.invalid[]
    """
    Then failed with the message:
    """
    The instance of 'java.lang.Object' is not a list
    """
    And got the following notation:
    """
      list.invalid[]
                  ^
    """
