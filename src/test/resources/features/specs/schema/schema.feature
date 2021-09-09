Feature: schema verification

  Scenario: verify json data is schema
    Given the following schema:
    """
    public class IdZero {
        public int id = 0;
    }
    """
    When the following input data:
    """
      {
        "id": 0
      }
    """
    Then the following assertion should pass:
    """
      is IdZero
    """
