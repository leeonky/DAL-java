Feature: arg range in currying

  Scenario: support args range
    Given the following java class:
    """
    public class Data {
      public String upperCase(String input) {
        return input.toUpperCase();
      }
    }
    """
    And args range of java class "Data" method "upperCase":
    """
    [
      {"java.lang.String": ["a", "b", "c"]}
    ]
    """
    When use a instance of java class "Data" to evaluate:
    """
    upperCase= {
      a: A
    }
    """
    Then failed with the message:
    """
    Unexpected fields `b`, `c` in upperCase
    """
    And got the following notation:
    """
    upperCase= {
             ^
      a: A
    }
    """

  Scenario: support args range in static method currying
    Given the following java class:
    """
    public class Data {
    }
    """
    And the following java class:
    """
    public class DataMethods {
      public static String upperCase(Data data, String input) {
        return input.toUpperCase();
      }
    }
    """
    And args range of java class "Data" static method "DataMethods"::"upperCase":
      | a | b | c |
    When use a instance of java class "Data" to evaluate:
    """
    upperCase= {
      a: A
    }
    """
    Then failed with the message:
    """
    Unexpected fields `b`, `c` in upperCase
    """
    And got the following notation:
    """
    upperCase= {
             ^
      a: A
    }
    """

  Scenario: args range in override method
    Given the following java class:
    """
    public class Data {
      public Object upperCase(int input) {
        return input;
      }

      public Object upperCase(String input) {
        return input;
      }
    }
    """
    And args range of java class "Data" method "upperCase":
    """
    [{
      "java.lang.String": ["a", "b", "c"]
    }]
    """
    When use a instance of java class "Data" to evaluate:
    """
    upperCase= {
      a: a
    }
    """
    Then failed with the message:
    """
    Unexpected fields `b`, `c` in upperCase
    """
    And got the following notation:
    """
    upperCase= {
             ^
      a: a
    }
    """

  Scenario: args range in multi arg methods
    Given the following java class:
    """
    public class Data {
      public Object upperCase(String arg1, String arg2, String arg3) {
        return arg1 + arg2 + arg3;
      }
    }
    """
    And args range of java class "Data" method "upperCase":
    """
    [
      {"java.lang.String": ["a", "b", "c"]},
      {"java.lang.String": ["x", "y"]},
      {"java.lang.String": ["m", "n"]}
    ]
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    upperCase= {
      a = {
        x = {
          m: axm
          n: axn
        }
        y = {
          m: aym
          n: ayn
        }
      }
      b = {
        x = {
          m: bxm
          n: bxn
        }
        y = {
          m: bym
          n: byn
        }
      }
      c = {
        x = {
          m: cxm
          n: cxn
        }
        y = {
          m: cym
          n: cyn
        }
      }
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    upperCase= {
      a = {
        x = {
          m: axm
          n: axn
        }
        y = {
          m: aym
          n: ayn
        }
      }
      c = {
        x = {
          m: cxm
          n: cxn
        }
        y = {
          m: cym
          n: cyn
        }
      }
    }
    """
    Then failed with the message:
    """
    Unexpected fields `b` in upperCase
    """
    And got the following notation:
    """
    upperCase= {
             ^
      a = {
        x = {
          m: axm
          n: axn
        }
        y = {
          m: aym
          n: ayn
        }
      }
      c = {
        x = {
          m: cxm
          n: cxn
        }
        y = {
          m: cym
          n: cyn
        }
      }
    }
    """

  Scenario: auto convent to right type in args range
    Given the following java class:
    """
    public class Data {
      public Object time(java.time.Instant instant, java.lang.Integer second) {
        return instant.plusSeconds(second);
      }
    }
    """
    And args range of java class "Data" method "time":
    """
    [
      {"java.time.Instant": ["1999-10-11T20:00:00Z", "2000-10-11T20:00:00Z"]},
      {"java.lang.Integer": [0, 1]}
    ]
    """
    Then the following verification for the instance of java class "Data" should pass:
    """
    time= {
      '1999-10-11T20:00:00Z'= {
        0: '1999-10-11T20:00:00Z'
        1: '1999-10-11T20:00:01Z'
      }
      '2000-10-11T20:00:00Z'= {
        0: '2000-10-11T20:00:00Z'
        1: '2000-10-11T20:00:01Z'
      }
    }
    """
    When use a instance of java class "Data" to evaluate:
    """
    time= {
      '1999-10-11T20:00:00Z'= {
        0: '1999-10-11T20:00:00Z'
        1: '1999-10-11T20:00:01Z'
      }
    }
    """
    Then failed with the message:
    """
    Unexpected fields 2000-10-11T20:00:00Z in time
    """
    And got the following notation:
    """
    time= {
        ^
      '1999-10-11T20:00:00Z'= {
        0: '1999-10-11T20:00:00Z'
        1: '1999-10-11T20:00:01Z'
      }
    }
    """
