Feature: access object property by ['xxx']

  Scenario Outline: access input object
    Given the following json:
    """
      {
        "id": 100,
        "first name": "Li"
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
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | code             | value | inspect        |
      | [ "id" ]         | 100   | ['id']         |
      | [ 'first name' ] | 'Li'  | ['first name'] |

  Scenario: raise error when access invalid property
    Given the following java class:
    """
    public class Data {
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
      ['invalid']
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
    Method or property `invalid` does not exist in `#package#Data`
    """
    And got the following notation:
    """
      ['invalid']
      ^
    """

  Scenario: raise error when object is null
    When evaluate by:
    """
    null['any']
    """
    Then failed with the message:
    """
    Instance is null
    """
    And got the following notation:
    """
    null['any']
        ^
    """

  Scenario: access number property via [number]
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, String> data = new java.util.HashMap<Object, String>() {{
        put(0, "str1");
        put(2, "str2");
        put("a", "strA");
      }};
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data[0]= "str1" and data[2]= "str2"
    """
    And the inspect should:
    """
    data[0]= 'str1' and data[2]= 'str2'
    """
    And the following verification for the instance of java class "Data" should pass:
    """
    data= {
      0= str1
      2= str2
      a= strA
    }
    """
    And the inspect should:
    """
    data= {0= 'str1', 2= 'str2', a= 'strA'}
    """

  Scenario Outline: support relax property name in []
    Given the following json:
    """
      {
        "<key>": 100,
        "first name": "Li"
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
    And the inspect should:
    """
    <inspect>
    """
    Examples:
      | key                        | code                           | value | inspect                        |
      | id                         | [ id ]                         | 100   | ['id']                         |
      | with space                 | [ with space ]                 | 100   | ['with space']                 |
      | !@#$%^&*(){}~`\|=+/?_-,.<> | [ !@#$%^&*(){}~`\|=+/?_-,.<> ] | 100   | ['!@#$%^&*(){}~`\|=+/?_-,.<>'] |
