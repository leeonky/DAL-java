Feature: string

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
