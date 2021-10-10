Feature: number and types

  Background: java class
    Given the following input java class data:
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

  Scenario: supported number types in value judgement
    Then the following assertion for "Bean" should pass:
    """
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
    """
    Then the following assertion for "Bean" should pass:
    """
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
    """

  Scenario: number1=number2 means number1 and number2 are the same type
    When assert "Bean" by the following code:
    """
      byteValue = 1
    """
    Then failed with the following message:
    """
    expected [1] equal to [1] but was not
    """
