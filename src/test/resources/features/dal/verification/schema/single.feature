Feature: single schema when verification failed

  Background:
    Given the following schema class:
    """
    public class IdZero {
        public int id = 0;
    }
    """

  Scenario: verify data matches single schema
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is IdZero
    """

  Scenario: raise error when unexpected field
    When the following json:
    """
      {
        "id": 0,
        "unexpected field": 1
      }
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expecting to match schema `IdZero` but was not
        Unexpected field `unexpected field` for schema IdZero[IdZero]
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when expected field is not exist
    When the following json:
    """
      {}
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expecting to match schema `IdZero` but was not
        Expecting field `id` to be in type IdZero[IdZero], but does not exist
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when schema field type is not matched
    When the following json:
    """
      {
        "id": "0"
      }
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expecting to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.String[0]
    """
    And got the following notation:
    """
      is IdZero
         ^
    """

  Scenario: raise error when schema field value is not matched
    When the following json:
    """
      {
        "id": 2
      }
    """
    When evaluate by:
    """
      is IdZero
    """
    Then failed with the message:
    """
    Expecting to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[2]
    """
    And got the following notation:
    """
      is IdZero
         ^
    """
