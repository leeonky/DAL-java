Feature: basic data and type

  Scenario Outline: value with keyword
    When evaluate by:
    """
    <code>
    """
    Then the result should:
    """
    = <value>
    """
    Examples:
      | code  | value |
      | true  | true  |
      | false | false |
      | null  | null  |

  Scenario: number types
    Given the following java class:
    """
    public class Numbers {
      public byte byteValue = 0;
      public Byte boxedByteValue = 0;
      public short shortValue = 1;
      public Short boxedShortValue = 1;
      public int intValue = 2;
      public Integer boxedIntValue = 2;
      public long longValue = 3;
      public Long boxedLongValue = 3l;
      public float floatValue = 4.0f;
      public Float boxedFloatValue = 4.0f;
      public double doubleValue = 5.0;
      public Double boxedDoubleValue = 5.0;
      public BigInteger bigInteger = new BigInteger("6");
      public BigDecimal bigDecimal = new BigDecimal("7.0");
      public boolean booleanValue = true;
      public Boolean boxedBooleanValue = false;
    }
    """
    Then the following verification for the instance of java class "Numbers" should pass:
    """
    : {
      byteValue = 0y
      boxedByteValue = 0Y
      shortValue = 1s
      boxedShortValue = 1S
      intValue = 2
      boxedIntValue = 2
      longValue = 3l
      boxedLongValue = 3L
      floatValue = 4.0f
      boxedFloatValue = 4.0F
      doubleValue = 5.0d
      boxedDoubleValue = 5.0D
      bigInteger = 6bi
      bigInteger = 6BI
      bigDecimal = 7.0bd
      bigDecimal = 7.0BD
      booleanValue = true
      boxedBooleanValue = false
    }
    """

  Scenario: number postfixes
    Given the following java class:
    """
    public class Numbers {
      public byte byteValue = 16;
      public Byte boxedByteValue = 16;
      public short shortValue = 17;
      public Short boxedShortValue = 17;
      public int intValue = 18;
      public Integer boxedIntValue = 18;
      public long longValue = 19;
      public Long boxedLongValue = 19l;
      public BigInteger bigInteger = new BigInteger("20");
    }
    """
    Then the following verification for the instance of java class "Numbers" should pass:
    """
    : {
      byteValue = 0x10y
      shortValue = 0x11s
      intValue = 0x12
      longValue = 0x13l
      bigInteger = 0x14bi
    }
    """

  Scenario: use proper type (int, long, biginteger) to hold int value
    * the following verification should pass:
    """
    0x80000000 = 0x80000000L,
    0x7fffffffffffffff = 0x7fffffffffffffffL,
    0x80000000000000000 = 0x80000000000000000BI
    """

  Scenario: use proper type (double, big decimal) to hold float value
    * the following verification should pass:
    """
    0.1 = 0.1D,
    2.7976931348623157e308 = 2.7976931348623157e308bd,
    -2.7976931348623157e10308 = -2.7976931348623157e10308bd
    """

  Scenario: String values
    Given the following java class:
    """
    public class Strings {
      public String value1="abc\n\tA'\"";
      public String value2="abc\n\r\tA'\"";
    }
    """
    Then the following verification for the instance of java class "Strings" should pass:
    """
    : {
      value1= 'abc
    	A\'"'
      value2= "abc\n\r\tA'\""
    }
    """

  Scenario: define and use user defined literal
    Given defined US dollar money object with the following regex
    """
    ^\$\d+
    """
    When evaluate by:
    """
    $1
    """
    Then the result should:
    """
      class.simpleName: 'USDollar'
    """
