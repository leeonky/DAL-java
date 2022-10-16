Feature: two dimensional array

  Scenario: two-dimensional array
    Given the following java class:
    """
    public class Data {
      public java.util.List<Object> data = java.util.Arrays.asList(java.util.Arrays.asList("str1", "str2", "strA"),
      java.util.Arrays.asList("str3", "str4", "strB"));
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data: | 0    | 1    | 2    |
          | str1 | str2 | strA |
          | str3 | str4 | strB |
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    data: ^| str1 | str2 | strA |
           | str3 | str4 | strB |
    """
    And the inspect should:
    """
    data: ^| : 'str1' | : 'str2' | : 'strA' |
    | : 'str3' | : 'str4' | : 'strB' |
    """
    When use a instance of java class "Data" to evaluate:
    """
    data: ^| str1 | str2 | strA |
           | str3 | str4 |
    """
    Then failed with the message:
    """
    Different list size
    Expected: <2>
    Actual: <3>
    """
    And got the following notation:
    """
    data: ^| str1 | str2 | strA |
           | str3 | str4 |
    ^^^^^^^^^^^^^^^^^^^^^^^
    """
    When use a instance of java class "Data" to evaluate:
    """
    data: ^| str1  | str2 | strA |
           | error | str4 | strB |
    """
    Then failed with the message:
    """
    Expected to match: java.lang.String
    <error>
     ^
    Actual: java.lang.String
    <str3>
     ^
    """
    And got the following notation:
    """
    data: ^| str1  | str2 | strA |
           | error | str4 | strB |
             ^
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    """
