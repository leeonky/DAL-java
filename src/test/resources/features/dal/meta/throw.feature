Feature: meta ::throw

  Scenario: return exception
    Given the following java class:
    """
    public class Data {
      public void test() {
        throw new java.lang.IndexOutOfBoundsException();
      }
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
      test::throw.class.simpleName=  IndexOutOfBoundsException
    """

  Scenario: raise error when no throw in meta throw
    Given the following java class:
    """
    public class Data {
      public void test() {
      }
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
      test::throw: {...}
    """
    Then failed with the message:
    """
    Expecting an error to be thrown, but nothing was thrown
    """
    And got the following notation:
    """
      test::throw: {...}
            ^
    """
