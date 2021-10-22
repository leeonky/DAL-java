Feature: schema verification

  Scenario: verify data matches schema
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
    When the following input data:
    """
      {
        "id": 0,
        "unexpected field": 1
      }
    """
    And assert by the following code:
    """
      is IdZero
    """
    Then failed with the following message:
    """
    Expecting to match schema `IdZero` but was not
        Unexpected field `unexpected field` for schema IdZero[IdZero]
    """
    When the following input data:
    """
      {}
    """
    And assert by the following code:
    """
      is IdZero
    """
    Then failed with the following message:
    """
    Expecting to match schema `IdZero` but was not
        Expecting field `id` to be type IdZero[IdZero], but does not exist
    """
    When the following input data:
    """
      {
        "id": "0"
      }
    """
    And assert by the following code:
    """
      is IdZero
    """
    Then failed with the following message:
    """
    Expecting to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.String[0]
    """

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

  Scenario: support element schema
    Given the following schema:
    """
    public class IdZero {
        public int id = 0;
    }
    """
    When the following input data:
    """
      [{
        "id": 0
      }, {
        "id": 0
      }]
    """
    Then the following assertion should pass:
    """
      is [IdZero]
    """
    When the following input data:
    """
      [{
        "id": 0
      }, {
        "id": 1
      }]
    """
    And assert by the following code:
    """
    is [IdZero]
    """
    Then failed with the following message:
    """
    Expecting element[1] to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following source code information:
    """
    is [IdZero]
        ^
    """
