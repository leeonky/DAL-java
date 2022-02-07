Feature: schema verification

  Scenario: verify data matches schema and which clause
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
      is IdZero which .id=0
    """
    And the following assertion should pass:
    """
      is IdZero which = {
        id: 0
      }
    """
    And the following assertion should pass:
    """
      is IdZero = {
        id: 0
      }
    """

#    TODO support field alias in multi schema list: is A / B
#    TODO support field alias in nested schema: is A is B
