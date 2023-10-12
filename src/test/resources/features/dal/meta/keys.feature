Feature: keys

  Scenario: support to get all keys as a list
    Given the following java class:
    """
    public class Bean {
      public String key1, key2;
    }
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    ::keys= [key1 key2]
    """
