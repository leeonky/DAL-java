Feature: dump-data

  Scenario Outline: dump single value
    Given the following json:
    """
    <value>
    """
    Then dumped data should be:
    """
    <expected>
    """
    Examples:
      | value   | expected                 |
      | null    | null                     |
      | 100.1   | java.lang.Double <100.1> |
      | "hello" | java.lang.String <hello> |

  Scenario: dump empty value
    Given register Empty value dumper;
    Given the following dal input:
    """
    Empty
    """
    Then dumped data should be:
    """
    com.github.leeonky.dal.compiler.IntegrationTestContext$Empty
    """
    Given the following dal inputs:
    """
    Empty
    Empty
    """
    Then dumped data should be:
    """
    [
        com.github.leeonky.dal.compiler.IntegrationTestContext$Empty,
        com.github.leeonky.dal.compiler.IntegrationTestContext$Empty
    ]
    """

  Scenario: dump list
    Given the following json:
    """
    [ 1, 2, "3", null]
    """
    Then dumped data should be:
    """
    [
        java.lang.Integer <1>,
        java.lang.Integer <2>,
        java.lang.String <3>,
        null
    ]
    """

  Scenario: dump empty list
    Given the following json:
    """
    []
    """
    Then dumped data should be:
    """
    []
    """

  Scenario: dump nested list
    Given the following json:
    """
    [[1], [1, 2], []]
    """
    Then dumped data should be:
    """
    [
        [
            java.lang.Integer <1>
        ],
        [
            java.lang.Integer <1>,
            java.lang.Integer <2>
        ],
        []
    ]
    """

  Scenario: dump object
    Given the following json:
    """
    {
      "name": "John"
    }
    """
    Then dumped data should be:
    """
    {
        name: java.lang.String <John>
    }
    """

  Scenario: empty object
    Given the following json:
    """
    { }
    """
    Then dumped data should be:
    """
    {}
    """

  Scenario: nested object
    Given the following json:
    """
    {
      "a": {
        "name": "John"
      },
      "b": {
        "name": "Tom"
      },
      "c": {}
    }
    """
    Then dumped data should be:
    """
    {
        a: {
            name: java.lang.String <John>
        },
        b: {
            name: java.lang.String <Tom>
        },
        c: {}
    }
    """

  Scenario: list of object
    Given the following json:
    """
    [
      {
        "name": "John"
      },
      {
         "name": "Tom"
      },
      {},
      {
        "name": "Jerry"
      }
    ]
    """
    Then dumped data should be:
    """
    [
        {
            name: java.lang.String <John>
        },
        {
            name: java.lang.String <Tom>
        },
        {},
        {
            name: java.lang.String <Jerry>
        }
    ]
    """

  Scenario: circle reference
    Given the following java class:
    """
    public class Data {
      public int value = 1;
      public Data getThis() {
        return this;
      }
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    #package#Data {
        value: java.lang.Integer <1>,
        this: *reference* root
    }
    """

  Scenario: circle reference of sub object
    Given the following java class:
    """
    public class Data {
      public int value = 1;
      public SubData subData = new SubData();
    }
    """
    And the following java class:
    """
    public class SubData {
      public SubData getThis() {
        return this;
      }
      public int value = 2;
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    #package#Data {
        value: java.lang.Integer <1>,
        subData: #package#SubData {
            value: java.lang.Integer <2>,
            this: *reference* root.subData
        }
    }
    """

  Scenario: should not be circle reference for empty map
    Given the following json:
    """
    { "a": {}, "b": {}, "c": {} }
    """
    Then dumped data should be:
    """
    {
        a: {},
        b: {},
        c: {}
    }
    """

  Scenario: same reference of list
    Given the following json:
    """
    [[{ "a": 1 }], [{ "a": 1 }]]
    """
    Then dumped data should be:
    """
    [
        [
            {
                a: java.lang.Integer <1>
            }
        ],
        [
            {
                a: java.lang.Integer <1>
            }
        ]
    ]
    """

  Scenario: ignore empty list in list reference
    Given the following json:
    """
    [[], [], []]
    """
    Then dumped data should be:
    """
    [
        [],
        [],
        []
    ]
    """

  Scenario: complex data dump
    Given the following json:
    """
    [{
      "obj": [{
        "user": {
          "name": "John"
         }
      }]
    }, {
      "name": "Tom"
    }, {
       "name": "John"
    }]
    """
    Then dumped data should be:
    """
    [
        {
            obj: [
                {
                    user: {
                        name: java.lang.String <John>
                    }
                }
            ]
        },
        {
            name: java.lang.String <Tom>
        },
        {
            name: java.lang.String <John>
        }
    ]
    """

  Scenario: build-in value type:
    Given the following java class:
    """
    public class Data {
      public boolean booleanValue = true;
      public Boolean boxedBoolean = false;
      public java.util.UUID uuid = java.util.UUID.fromString("00000000-0000-0000-0000-000000000000");
      public java.time.Instant instant = java.time.Instant.parse("2000-01-01T00:00:00Z");
      public java.util.Date date = java.util.Date.from(java.time.Instant.parse("2000-01-02T00:00:00Z"));
      public java.time.LocalTime localTime = java.time.LocalTime.parse("12:00:00");
      public java.time.LocalDate localDate = java.time.LocalDate.parse("2000-12-01");
      public java.time.LocalDateTime localDateTime = java.time.LocalDateTime.parse("2000-12-01T12:00:00");
      public java.time.OffsetDateTime offsetDateTime = java.time.OffsetDateTime.parse("1996-01-23T00:00:01+08:00");
      public java.time.ZonedDateTime zonedDateTime = java.time.ZonedDateTime.parse("1996-01-23T00:00:01+08:00[Asia/Shanghai]");
      public java.time.YearMonth yearMonth = java.time.YearMonth.parse("2000-12");
      public Class type = java.lang.String.class;
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    #package#Data {
        booleanValue: java.lang.Boolean <true>,
        boxedBoolean: java.lang.Boolean <false>,
        uuid: java.util.UUID <00000000-0000-0000-0000-000000000000>,
        instant: java.time.Instant <2000-01-01T00:00:00Z>,
        date: java.util.Date <Sun Jan 02 00:00:00 UTC 2000>,
        localTime: java.time.LocalTime <12:00>,
        localDate: java.time.LocalDate <2000-12-01>,
        localDateTime: java.time.LocalDateTime <2000-12-01T12:00>,
        offsetDateTime: java.time.OffsetDateTime <1996-01-23T00:00:01+08:00>,
        zonedDateTime: java.time.ZonedDateTime <1996-01-23T00:00:01+08:00[Asia/Shanghai]>,
        yearMonth: java.time.YearMonth <2000-12>,
        type: java.lang.Class <class java.lang.String>
    }
    """

  Scenario: ignore exception during dump
    And the following java class:
    """
    public class Data {
      public int value = 2;
      public int getError() {
        throw new java.lang.RuntimeException("error");
      }
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    #package#Data {
        value: java.lang.Integer <2>,
        error: *throw* com.github.leeonky.dal.runtime.PropertyAccessException: com.github.leeonky.util.InvocationException: java.lang.RuntimeException: error
    }
    """

  Scenario: dump number key
    Given the following java class:
    """
    public class Data {
      public java.util.Map<Object, String> data = new java.util.HashMap<Object, String>() {{
        put(0, "str1");
      }};
    }

    """
    Then dumped instance of java class "Data" should be:
    """
    #package#Data {
        data: {
            0: java.lang.String <str1>
        }
    }
    """

  Scenario: max dump lines
    Given the following json:
    """
    {
      "name": "John",
      "age": 18,
      "comments": "blabla"
    }
    """
    Then dumped data under 3 lines should be:
    """
    {
        name: java.lang.String <John>,
        age: java.lang.Integer <18>
    ...
    """

  Scenario: dump stackTraceItem array
    Given the following java class:
    """
    public class Data {
      public StackTraceElement[] stacks = new StackTraceElement[] {
        new StackTraceElement("class", "method", "file", 1),
        new StackTraceElement("class", "method", "file", 2),
        new StackTraceElement("class", "method", "file", 3),
        new StackTraceElement("class", "method", "file", 4)
      };
    }
    """
    Then dumped instance of java class "Data" should be:
    """
    #package#Data {
        stacks: 
            at class.method(file:1)
            at class.method(file:2)
            at class.method(file:3)
            at class.method(file:4)
    }
    """

  Scenario: should ignore dump error
    Given the following java class:
    """
    public class Data {
      {
        com.github.leeonky.dal.DAL.getInstance().getRuntimeContextBuilder().registerDumper(Data.class, data ->
        new com.github.leeonky.dal.runtime.inspector.Dumper(){
          public void dump(com.github.leeonky.dal.runtime.Data data,
            com.github.leeonky.dal.runtime.inspector.DumpingBuffer dumpingBuffer) {
            throw new java.lang.RuntimeException("Error");
          }
        });
      }
      public int getError() {
        return 1;
      }
    }
    """
    Given the following java class:
    """
    public class AData {
      public Data getData() {
        return new Data();
      }
    }
    """
    When use a instance of java class "Data" to assert:
    """
    = ''
    """
    Then got the following exception:
    """
    message.trim= ```
                  = ''
                    ^

                  Expected to be equal to: java.lang.String
                                           ^
                  <>
                  Actual: *dump throw* java.lang.RuntimeException: Error
                          ^

                  The root value was: *dump throw* java.lang.RuntimeException: Error
                  ```
    """
