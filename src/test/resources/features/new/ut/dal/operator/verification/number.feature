Feature: number

  Scenario: verify number with '=' and ':', operator ':' can compare number in different type
    Given the following java class:
    """
    public class Bean {
        public byte byteValue = 1;
        public Byte boxedByteValue = 1;
        public short shortValue = 1;
        public short boxedShortValue = 1;
        public int intValue = 1;
        public int boxedIntValue = 1;
        public long longValue = 1;
        public Long boxedLongValue = 1L;
        public float floatValue = 1;
        public Float boxedFloatValue = 1F;
        public double doubleValue = 1;
        public Double boxedDoubleValue = 1D;
        public BigInteger bigIntegerValue = BigInteger.valueOf(1);
        public BigDecimal bigDecimalValue = BigDecimal.valueOf(1);
    }
    """
    Then the following verification for the instance of java class "Bean" should pass:
    """
    : {
      byteValue= 1Y
      boxedByteValue= 1y
      shortValue= 1S
      boxedShortValue= 1s
      intValue= 1
      boxedIntValue= 1
      longValue= 1L
      boxedLongValue= 1l
      floatValue= 1F
      boxedFloatValue= 1f
      doubleValue= 1D
      boxedDoubleValue= 1d
      bigIntegerValue= 1bi
      bigDecimalValue= 1bd
    }
    """
    And the following verification for the instance of java class "Bean" should pass:
    """
    : {
      byteValue: 1
      boxedByteValue: 1
      shortValue: 1
      boxedShortValue: 1
      intValue: 1.0
      boxedIntValue: 1.0
      longValue: 1
      boxedLongValue: 1
      floatValue: 1
      boxedFloatValue: 1
      doubleValue: 1
      boxedDoubleValue: 1
      bigIntegerValue: 1
      bigDecimalValue: 1
    }
    """

  Scenario: number verification failed in same type
    Given evaluate by:
    """
      5 = 4
    """
    Then failed with the message:
    """
    Expecting java.lang.Integer
    <5>
    to be equal to java.lang.Integer
    <4>
    but was not
    """
    And got the following notation:
    """
      5 = 4
          ^
    """

  Scenario: number verification failed in same value but different type
    Given evaluate by:
    """
      5 = 5.0
    """
    Then failed with the message:
    """
    Expecting java.lang.Integer
    <5>
    to be equal to java.lang.Double
    <5.0>
    but was not
    """
    And got the following notation:
    """
      5 = 5.0
          ^
    """

  Scenario: number verification failed in same value but different type
    Given evaluate by:
    """
      5: 4.0
    """
    Then failed with the message:
    """
    Expecting java.lang.Integer
    <5>
    to match java.lang.Double
    <4.0>
    but was not
    """
    And got the following notation:
    """
      5: 4.0
         ^
    """

  Scenario: not allow convert string to number implicitly
    Given evaluate by:
    """
      '5': 5
    """
    Then failed with the message:
    """
    Cannot compare between java.lang.String
    <5>
    and java.lang.Integer
    <5>

    """
    And got the following notation:
    """
      '5': 5
         ^
    """
