package com.github.leeonky.interpreter;

import java.math.BigDecimal;
import java.math.BigInteger;

public class NumberParser {
    private static final PrimitiveIntegerPostfix BYTE_POSTFIX = new PrimitiveIntegerPostfix(1) {
        @Override
        public Number convertFrom(int number, String content) {
            if (number > Byte.MAX_VALUE || number < Byte.MIN_VALUE)
                throw new NumberOverflowException(content);
            return (byte) number;
        }
    }, SHORT_POSTFIX = new PrimitiveIntegerPostfix(1) {
        @Override
        public Number convertFrom(int number, String content) {
            if (number > Short.MAX_VALUE || number < Short.MIN_VALUE)
                throw new NumberOverflowException(content);
            return (short) number;
        }
    }, LONG_POSTFIX = new PrimitiveIntegerPostfix(1) {
        @Override
        public Number convertFrom(int number, String content) {
            return (long) number;
        }

        @Override
        public Number convertFrom(long number, String content) {
            return number;
        }
    };
    private static final StringNumberPostfix BIG_INTEGER_POSTFIX = new StringNumberPostfix(2) {
        @Override
        public Number convertFromBigInteger(String numberString, int radix, String content) {
            return new BigInteger(numberString, radix);
        }
    }, FLOAT_POSTFIX = new StringNumberPostfix(1) {
        @Override
        public Number convertFromDecimal(String numberString, String content) {
            return verifyInfinite(Float.parseFloat(numberString), content);
        }

        private Number verifyInfinite(float f, String content) {
            if (Float.isInfinite(f))
                throw new NumberOverflowException(content);
            return f;
        }

        @Override
        public Number convertFromBigInteger(String numberString, int radix, String content) {
            return verifyInfinite(Float.parseFloat(numberString), content);
        }
    }, DOUBLE_POSTFIX = new StringNumberPostfix(1) {
        @Override
        public Number convertFromDecimal(String numberString, String content) {
            return verifyInfinite(Double.parseDouble(numberString), content);
        }

        @Override
        public Number convertFromBigInteger(String numberString, int radix, String content) {
            return verifyInfinite(Double.parseDouble(numberString), content);
        }

        private Number verifyInfinite(double d, String content) {
            if (Double.isInfinite(d))
                throw new NumberOverflowException(content);
            return d;
        }
    }, BIG_DECIMAL_POSTFIX = new StringNumberPostfix(2) {
        @Override
        public Number convertFromDecimal(String numberString, String content) {
            return new BigDecimal(numberString);
        }

        @Override
        public Number convertFromBigInteger(String numberString, int radix, String content) {
            return new BigDecimal(numberString);
        }
    };

    static class StringNumberPostfix {
        public final int length;

        public StringNumberPostfix(int length) {
            this.length = length;
        }

        public Number convertFromBigInteger(String numberString, int radix, String content) {
            throw new NumberOverflowException(content);
        }

        public Number convertFromDecimal(String numberString, String content) {
            throw new NumberOverflowException(content);
        }
    }

    static abstract class PrimitiveIntegerPostfix extends StringNumberPostfix {
        protected PrimitiveIntegerPostfix(int length) {
            super(length);
        }

        public abstract Number convertFrom(int number, String content);

        public Number convertFrom(long number, String content) {
            throw new NumberOverflowException(content);
        }
    }

    public Number parse(String content) {
        if (content == null)
            return null;
        int length = content.length();
        if (length == 0)
            return null;
        int sign = 1;
        int index = 0;
        char c = content.charAt(index);
        if (c == '+') {
            if (++index == length)
                return null;
        } else if (c == '-') {
            if (++index == length)
                return null;
            sign = -1;
        }
        int radix = 10;
        if (index + 1 < length && content.charAt(index) == '0') {
            char radixChar = content.charAt(index + 1);
            if (isDigit(radixChar) || radixChar == '_') {
                index++;
                radix = 8;
            } else if (index + 2 < length) {
                if (radixChar == 'x' || radixChar == 'X') {
                    index += 2;
                    radix = 16;
                } else if (radixChar == 'b' || radixChar == 'B') {
                    char nextChar = content.charAt(index + 2);
                    if (nextChar == '0' || nextChar == '1') {
                        index += 2;
                        radix = 2;
                    }
                }
            }
        }
        c = content.charAt(length - 1);
        StringNumberPostfix postfix = fetchDecimalOrBigIntegerPostfix(content, radix, c);
        if (postfix != null) {
            if (index == (length -= postfix.length))
                return null;
            return continueParseBigInteger(radix, index, content, length, postfix, newStringBuilder(length, sign));
        }
        return parseFromInteger(content, length, sign, index, radix, fetchOtherPostfix(c));
    }

    private PrimitiveIntegerPostfix fetchOtherPostfix(char c) {
        switch (c) {
            case 'y':
            case 'Y':
                return BYTE_POSTFIX;
            case 's':
            case 'S':
                return SHORT_POSTFIX;
            case 'l':
            case 'L':
                return LONG_POSTFIX;
        }
        return null;
    }

    private StringNumberPostfix fetchDecimalOrBigIntegerPostfix(String content, int radix, char c) {
        if (content.endsWith("bi") || content.endsWith("BI"))
            return BIG_INTEGER_POSTFIX;
        if (radix == 10) {
            if (content.endsWith("bd") || content.endsWith("BD"))
                return BIG_DECIMAL_POSTFIX;
            switch (c) {
                case 'f':
                case 'F':
                    return FLOAT_POSTFIX;
                case 'd':
                case 'D':
                    return DOUBLE_POSTFIX;
            }
        }
        return null;
    }

    private StringBuilder newStringBuilder(int length, int sign) {
        StringBuilder stringBuilder = new StringBuilder(length);
        if (sign == -1)
            stringBuilder.append('-');
        return stringBuilder;
    }

    private Number parseFromInteger(String content, int length, int sign, int index, int radix, PrimitiveIntegerPostfix postfix) {
        if (postfix != null) {
            if (index == (length -= postfix.length))
                return null;
        }
        int number = 0;
        int limit = sign == 1 ? -Integer.MAX_VALUE : Integer.MIN_VALUE;
        int limitBeforeMul = limit / radix;
        while (index < length) {
            char c = content.charAt(index++);
            if (c == '_' && index != length)
                continue;
            int digit = getDigit(radix, c);
            if (digit < 0) {
                if (isFloatDot(radix, c, index, length, content))
                    return parseDoubleWithDot(toStringBuilder(radix, sign, number, length), radix, content, index, length, postfix);
                if (isPowerChar(radix, c, index, length, content))
                    return parseDoubleWithPower(content, index, length, toStringBuilder(radix, sign, number, length), postfix);
                return null;
            }
            if (isOverflow(digit, number, limit, limitBeforeMul, radix))
                return continueParseLong(sign, radix, number, digit, index, content, length, postfix);
            number = number * radix - digit;
        }
        if (sign == 1)
            number = -number;
        return postfix != null ? postfix.convertFrom(number, content) : number;
    }

    private Number parseDoubleWithPower(String content, int index, int length, StringBuilder stringBuilder, StringNumberPostfix postfix) {
        stringBuilder.append('E');
        int eSign = 1;
        if (content.charAt(index) == '+') {
            if (++index == length)
                return null;
        }
        if (content.charAt(index) == '-') {
            if (++index == length)
                return null;
            eSign = -1;
        }
        if (eSign == -1)
            stringBuilder.append('-');
        while (index < length) {
            char c = content.charAt(index++);
            if (c == '_' && index != length)
                continue;
            if (notDigit(c))
                return null;
            stringBuilder.append(c);
        }
        return toDoubleOrBigDecimal(stringBuilder, postfix, content);
    }

    private boolean isPowerChar(int radix, char c, int index, int length, String content) {
        return (c == 'e' || c == 'E') && radix == 10
                && afterDigit(index, content) && beforeSignOrDigit(index, length, content);
    }

    private boolean beforeSignOrDigit(int index, int length, String content) {
        if (index >= length)
            return false;
        char c = content.charAt(index);
        return (isDigit(c) || c == '-' || c == '+');
    }

    private boolean isFloatDot(int radix, char c, int index, int length, String content) {
        return c == '.' && radix == 10 && afterDigit(index, content) && beforeDigit(index, length, content);
    }

    private boolean beforeDigit(int index, int length, String content) {
        return index < length && isDigit(content.charAt(index));
    }

    private boolean afterDigit(int index, String content) {
        return index > 1 && isDigit(content.charAt(index - 2));
    }

    private boolean notDigit(char c) {
        return c < '0' || c > '9';
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private Number parseDoubleWithDot(StringBuilder stringBuilder, int radix, String content, int index, int length, StringNumberPostfix postfix) {
        stringBuilder.append('.');
        while (index < length) {
            char c = content.charAt(index++);
            if (c == '_' && index != length)
                continue;
            if (isPowerChar(radix, c, index, length, content))
                return parseDoubleWithPower(content, index, length, stringBuilder, postfix);
            if (notDigit(c))
                return null;
            stringBuilder.append(c);
        }
        return toDoubleOrBigDecimal(stringBuilder, postfix, content);
    }

    private Number toDoubleOrBigDecimal(StringBuilder stringBuilder, StringNumberPostfix postfix, String content) {
        String numberString = stringBuilder.toString();
        if (postfix != null)
            return postfix.convertFromDecimal(numberString, content);
        double d = Double.parseDouble(numberString);
        if (Double.isInfinite(d))
            return new BigDecimal(numberString);
        return d;
    }

    private Number continueParseLong(int sign, int radix, long number, int digit, int index,
                                     String content, int length, PrimitiveIntegerPostfix postfix) {
        number = number * radix - digit;
        long limitLong = sign == 1 ? -Long.MAX_VALUE : Long.MIN_VALUE;
        long limitBeforeMulLong = limitLong / radix;
        while (index < length) {
            char c = content.charAt(index++);
            if (c == '_' && index != length)
                continue;
            digit = getDigit(radix, c);
            if (digit < 0) {
                if (isFloatDot(radix, c, index, length, content))
                    return parseDoubleWithDot(toStringBuilder(radix, sign, number, length), radix, content, index, length, postfix);
                if (isPowerChar(radix, c, index, length, content))
                    return parseDoubleWithPower(content, index, length, toStringBuilder(radix, sign, number, length), postfix);
                return null;
            }
            if (isOverflow(digit, number, limitLong, limitBeforeMulLong, radix))
                return continueParseBigInteger(radix, index, content, length, postfix,
                        toStringBuilder(radix, sign, number, length).append(c));
            number = number * radix - digit;
        }
        if (sign == 1)
            number = -number;
        return postfix == null ? number : postfix.convertFrom(number, content);
    }

    private Number continueParseBigInteger(int radix, int index, String content, int length,
                                           StringNumberPostfix postfix, StringBuilder stringBuilder) {
        while (index < length) {
            char c = content.charAt(index++);
            if (c == '_' && index != length)
                continue;
            int digit = getDigit(radix, c);
            if (digit < 0) {
                if (isFloatDot(radix, c, index, length, content))
                    return parseDoubleWithDot(stringBuilder, radix, content, index, length, postfix);
                if (isPowerChar(radix, c, index, length, content))
                    return parseDoubleWithPower(content, index, length, stringBuilder, postfix);
                return null;
            }
            stringBuilder.append(c);
        }
        return postfix == null ? new BigInteger(stringBuilder.toString(), radix)
                : postfix.convertFromBigInteger(stringBuilder.toString(), radix, content);
    }

    private StringBuilder toStringBuilder(int radix, int sign, long number, int length) {
        return newStringBuilder(length, sign).append(Long.toString(-number, radix));
    }

    private boolean isOverflow(int digit, int number, int limit, int limitBeforeMul, int radix) {
        return number < limitBeforeMul || number * radix < limit + digit;
    }

    private boolean isOverflow(int digit, long number, long limit, long limitBeforeMul, int radix) {
        return number < limitBeforeMul || number * radix < limit + digit;
    }

    private int getDigit(int radix, char c) {
        int value;
        if (c <= '9')
            value = c - '0';
        else if (c >= 'a')
            value = c - 'a' + 10;
        else
            value = c - 'A' + 10;
        if (value >= 0 && value < radix)
            return value;
        return -1;
    }
}
