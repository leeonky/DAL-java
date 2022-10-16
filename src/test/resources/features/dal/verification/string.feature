Feature: string

  Scenario Outline: compare string
    Then the following verification should pass:
    """
      'abc'<opt> 'abc'
    """
    And the following verification should failed:
    """
      'abc'<opt> 'efg'
    """
    Examples:
      | opt |
      | =   |
      | :   |

  Scenario: different string with =
    When evaluate by:
    """
      'abc'= 'efg'
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.String
    <efg>
     ^
    Actual: java.lang.String
    <abc>
     ^
    """
    And got the following notation:
    """
      'abc'= 'efg'
             ^
    """

  Scenario: different string with :
    When evaluate by:
    """
      'abc': 'efg'
    """
    Then failed with the message:
    """
    Expected to match: java.lang.String
    <efg>
     ^
    Actual: java.lang.String
    <abc>
     ^
    """
    And got the following notation:
    """
      'abc': 'efg'
             ^
    """

  Scenario: not string type with =
    When the following java class:
    """
    public class Data {
      public java.time.Instant instant = java.time.Instant.parse("2000-10-10T00:00:00Z");
    }
    """
    Then use a instance of java class "Data" to evaluate:
    """
      instant= '2000-10-10T00:00:00Z'
    """
    Then failed with the message:
    """
    Expected to be equal to: java.lang.String
                                  ^
    <2000-10-10T00:00:00Z>
    Actual: java.time.Instant
                 ^
    <2000-10-10T00:00:00Z>
    """
    And got the following notation:
    """
      instant= '2000-10-10T00:00:00Z'
               ^
    """

  Scenario: implicitly to string with :
    When the following java class:
    """
    public class Data {
      public java.time.Instant instant = java.time.Instant.parse("2000-10-10T00:00:00Z");
    }
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
      instant: '2000-10-10T00:00:00Z'
    """
    When use a instance of java class "Data" to evaluate:
    """
      instant: 'un matched'
    """
    And failed with the message:
    """
    Expected to match: java.lang.String
    <un matched>
     ^
    Actual: java.lang.String
    <2000-10-10T00:00:00Z> converted from: java.time.Instant
     ^
    <2000-10-10T00:00:00Z>
    """

  Scenario: not allow convert number to string implicitly
    When evaluate by:
    """
      5: '5'
    """
    Then failed with the message:
    """
    Cannot compare between java.lang.Integer
    <5>
    and java.lang.String
    <5>
    """
    And got the following notation:
    """
      5: '5'
         ^
    """

  Scenario: not allow convert boolean to string implicitly
    When evaluate by:
    """
      true: 'true'
    """
    Then failed with the message:
    """
    Cannot compare between java.lang.Boolean
    <true>
    and java.lang.String
    <true>
    """
    And got the following notation:
    """
      true: 'true'
            ^
    """
