Feature: multiple schema and list schema

  Background:
    Given the following schema class:
    """
    public class IdZero {
        public int id = 0;
    }
    """
    And the following schema class:
    """
    public class IdOne {
        public int id = 1;
    }
    """

  Scenario: verify data matches multiple schema
    And the following json:
    """
      {
        "id": 0
      }
    """
    Then the following verification should pass:
    """
      is IdZero / IdZero/IdZero
    """
    And the inspect should:
    """
    is IdZero / IdZero / IdZero
    """

  Scenario: raise error when any schema failed
    When the following json:
    """
      {
        "id": 0
      }
    """
    When evaluate by:
    """
      is IdZero / IdOne
    """
    Then failed with the message:
    """
    Expecting to match schema `IdOne` but was not
        Expecting field `.id` to be java.lang.Integer[1], but was java.lang.Integer[0]
    """
    And got the following notation:
    """
      is IdZero / IdOne
                  ^
    """
    When evaluate by:
    """
      is IdOne / IdZero
    """
    Then failed with the message:
    """
    Expecting to match schema `IdOne` but was not
        Expecting field `.id` to be java.lang.Integer[1], but was java.lang.Integer[0]
    """
    And got the following notation:
    """
      is IdOne / IdZero
         ^
    """

  Scenario: verify list data matches list schema
    Given the following json:
    """
      [{
        "id": 0
      }, {
        "id": 0
      }]
    """
    Then the following verification should pass:
    """
      is [IdZero]
    """
    And the following verification should pass:
    """
      is [IdZero/ IdZero]
    """
    And the inspect should:
    """
    is [IdZero / IdZero]
    """

  Scenario: raise error when one element is not matched schema
    Given the following json:
    """
      [{
        "id": 0
      }, {
        "id": 1
      }]
    """
    When evaluate by:
    """
    is [IdZero]
    """
    Then failed with the message:
    """
    Expecting [1] to match schema `IdZero` but was not
        Expecting field `.id` to be java.lang.Integer[0], but was java.lang.Integer[1]
    """
    And got the following notation:
    """
    is [IdZero]
        ^
    """

  Scenario: should raise error when input is not list
    When evaluate by:
    """
      1 is [IdZero]
    """
    Then failed with the message:
    """
    Expecting a list but was java.lang.Integer
    <1>
    """
    And got the following notation:
    """
      1 is [IdZero]
      ^
    """
