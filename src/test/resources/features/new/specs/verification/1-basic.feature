Feature: basic assertion

  Scenario Outline: number verification in same type
    Then the following verification should pass:
    """
      100<opt> 100
    """
    But the following verification should failed:
    """
      100<opt> 101
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: number eq should in same type, number match can be in different type
    Then the following verification should failed:
    """
      100= 100.0
    """
    But the following verification should pass:
    """
      100: 100.0
    """

  Scenario: not allow convert string to number implicitly
    * the following verification should failed:
    """
      '1': 1
    """

  Scenario Outline: String verification with both type string
    Then the following verification should pass:
    """
      'abc'<opt> 'abc'
    """
    But the following verification should failed:
    """
      'abc'<opt> 'efg'
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: implicitly to string when compare object and string with :
    Given the following java class:
    """
    public class Data {
      public java.time.Instant instant = java.time.Instant.parse("2000-10-10T00:00:00Z");
    }
    """
    Then the following verification for the instance of java class "Data" should failed:
    """
      instant= '2000-10-10T00:00:00Z'
    """
    But the following verification for the instance of java class "Data" should pass:
    """
      instant: '2000-10-10T00:00:00Z'
    """

  Scenario: not allow convert number and boolean to string implicitly
    * the following verification should failed:
    """
      1: '1'
    """
    * the following verification should failed:
    """
      true: 'true'
    """

  Scenario Outline: boolean verification
    Then the following verification should pass:
    """
      true <opt> true and false <opt> false
    """
    But the following verification should failed:
    """
      true <opt> false
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: not allow convert string to boolean implicitly
    * the following verification should failed:
    """
      'true': true
    """

  Scenario: wildcard verification
    Then the following verification should pass:
    """
      1: * and 1= * and null: * and null= *
    """
