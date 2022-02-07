Feature: access object property by .xxx

  Scenario Outline: access input object
    Given the following json:
    """
      {
        "id": 100,
        "name": "Tom"
      }
    """
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    : <value>
    """
    Examples:
      | code  | value |
      | .id   | 100   |
      | .name | 'Tom' |

  Scenario: do not allow empty property
    When evaluate by:
    """
    .
    """
    Then failed with the message:
    """
    expect a symbol
    """
    And got the following notation:
    """
    .
     ^
    """

  Scenario: do not allow empty property(has white space)
    When evaluate by:
    """
    . 
    """
    Then failed with the message:
    """
    expect a symbol
    """
    And got the following notation:
    """
    . 
      ^
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
    And got the following notation:
    """
      .invalid
       ^
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
