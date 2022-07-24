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
