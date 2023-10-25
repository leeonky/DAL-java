Feature: add

  Scenario: all supported build in collection
    Given the following java class:
    """
    public class Bean {
      public String[] array = new String[] {"hello", "world"};
      public List<String> list = Arrays.asList("hello", "world");
      public Collection<String> set = new LinkedHashSet<>(Arrays.asList("hello", "world"));
      public java.util.stream.Stream<String> stream() {
        return Arrays.asList("hello", "world").stream();
      }
    }
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    : {
      <<array, set, list, stream>>= [hello world]
      <<array, set, list, stream>>[0]= hello
      <<array, set, list, stream>>[1]= world
    }
    """

  Scenario: skip row should check size
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
     = >>| name | *** |
         | age  |     |
         | id   |     |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <1>
    Actual: <0>
    """
    And got the following notation:
    """
     = >>| name | *** |
       ^
         | age  |     |
         | id   |     |
    """

  Scenario: skip row should check size
    Given the following json:
    """
    []
    """
    When evaluate by:
    """
     = | name | age | id |
       | ***             |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <1>
    Actual: <0>
    """
    And got the following notation:
    """
     = | name | age | id |
       ^
       | ***             |
    """

#   TODO test list contains with field alias in element [... {aliasOfXX: 1}...]