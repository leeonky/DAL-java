package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class NumberParserTest {

    @Nested
    class IntegerNumber {

        @Nested
        class Radix10 {

            @Test
            void parse_int_number() {
                assertParse("0", 0);
                assertParse("1", 1);
                assertParse("2", 2);
                assertParse("3", 3);
                assertParse("4", 4);
                assertParse("5", 5);
                assertParse("6", 6);
                assertParse("7", 7);
                assertParse("8", 8);
                assertParse("9", 9);
                assertParse("24", 24);
                assertParse("1_000", 1_000);
                assertParse("+24", 24);
                assertParse("2147483647", 2147483647);
                assertParse("1234567890", 1234567890);
            }

            @Test
            void negative() {
                assertParse("-24", -24);
                assertParse("-2147483648", -2147483648);
            }

            @Test
            void over_flow() {
                assertParse("2147483648", 2147483648L);
                assertParse("2147483657", 2147483657L);
                assertParse("-2147483649", -2147483649L);
                assertParse("-2147483658", -2147483658L);
            }

            @Test
            void invalid_number() {
                assertParse(null, null);
                assertParse("+", null);
                assertParse("-", null);
                assertParse("1_", null);
                assertParse("1x", null);
                assertParse("F", null);
                assertParse("e", null);
                assertParse("y", null);
                assertParse("s", null);
                assertParse("l", null);
                assertParse("bi", null);
                assertParse("bd", null);
                assertParse("d", null);
                assertParse("-F", null);
                assertParse("-y", null);
                assertParse("-s", null);
                assertParse("-l", null);
                assertParse("-bi", null);
                assertParse("-bd", null);
                assertParse("-d", null);
            }
        }

        @Nested
        class Radix16 {

            @Test
            void parse_int_number() {
                assertParse("0x0", 0);
                assertParse("0x1", 1);
                assertParse("0x2", 2);
                assertParse("0x3", 3);
                assertParse("0x4", 4);
                assertParse("0x5", 5);
                assertParse("0x6", 6);
                assertParse("0x7", 7);
                assertParse("0x8", 8);
                assertParse("0x9", 9);
                assertParse("0xa", 0xa);
                assertParse("0xb", 0xb);
                assertParse("0xc", 0xc);
                assertParse("0xd", 0xd);
                assertParse("0xe", 0xe);
                assertParse("0xf", 0xf);
                assertParse("0XF", 0XF);
                assertParse("0xd", 0xd);
                assertParse("0xbd", 0xbd);
                assertParse("0x1_000", 0x1_000);
                assertParse("+0xff", 0xff);
                assertParse("0x7fffffff", 2147483647);
            }

            @Test
            void negative() {
                assertParse("-0x1f", -0x1f);
                assertParse("-0x80000000", -0x80000000);
            }

            @Test
            void over_flow() {
                assertParse("0x80000000", 0x80000000L);
                assertParse("-0x80000001", -0x80000001L);
            }

            @Test
            void invalid_number() {
                assertParse("0x", null);
                assertParse("+0x", null);
                assertParse("-0x", null);
                assertParse("0x1_", null);
                assertParse("0x1x", null);
                assertParse("0xG", null);
            }
        }

        @Nested
        class Radix2 {

            @Test
            void parse_int_number() {
                assertParse("0b0", 0);
                assertParse("0b1", 1);
                assertParse("0b10", 2);
                assertParse("0b1000_1000", 0x88);
            }

            @Test
            void negative() {
                assertParse("-0B10", -2);
            }

            @Test
            void over_flow() {
                assertParse("0b1000_0000_0000_0000_0000_0000_0000_0000", 0x80000000L);
                assertParse("-0b1000_0000_0000_0000_0000_0000_0000_0001", -0x80000001L);
            }

            @Test
            void invalid_number() {
                assertParse("0b", null);
                assertParse("+0b", null);
                assertParse("-0b", null);
                assertParse("0b1_", null);
                assertParse("0b1b", null);
                assertParse("0b2", null);
            }
        }

        @Nested
        class Radix8 {

            @Test
            void parse_int_number() {
                assertParse("00", 0);
                assertParse("01", 1);
                assertParse("02", 2);
                assertParse("03", 3);
                assertParse("04", 4);
                assertParse("05", 5);
                assertParse("06", 6);
                assertParse("07", 7);
                assertParse("010", 8);
                assertParse("0170", 120);
                assertParse("+0170", 120);
                assertParse("+0_171", 121);
                assertParse("0_172", 122);
            }

            @Test
            void negative() {
                assertParse("-0170", -120);
                assertParse("-0_171", -121);
                assertParse("-0_172", -122);
            }

            @Test
            void over_flow() {
                assertParse("020000000000", 0x80000000L);
                assertParse("-020000000001", -0x80000001L);
            }

            @Test
            void invalid_number() {
                assertParse("08", null);
                assertParse("07_", null);
            }
        }
    }

    @Nested
    class ParseLong {

        @Nested
        class Radix10 {

            @Test
            void parse_long() {
                assertParse("100000000005", 100000000005L);
                assertParse("100000000005_000", 100000000005_000L);
                assertParse("9223372036854775807", 9223372036854775807L);
            }

            @Test
            void negative() {
                assertParse("0x80000010", 0x80000010L);
                assertParse("-0x80000010", -0x80000010L);
                assertParse("-9223372036854775808", -9223372036854775808L);
            }

            @Test
            void over_flow() {
                assertParse("9223372036854775808", new BigInteger("9223372036854775808"));
                assertParse("9223372036854775811", new BigInteger("9223372036854775811"));
                assertParse("-9223372036854775809", new BigInteger("-9223372036854775809"));
                assertParse("-9223372036854775811", new BigInteger("-9223372036854775811"));
            }

            @Test
            void invalid_number() {
                assertParse("100000000005_", null);
                assertParse("100000000005xx", null);
            }
        }

        @Nested
        class Radix16 {

            @Test
            void parse_long() {
                assertParse("0xfffffffffff", 0xfffffffffffL);
                assertParse("0xfff_ffff_ffff", 0xfff_ffff_ffffL);
                assertParse("0x7fffffffffffffff", 9223372036854775807L);
            }

            @Test
            void negative() {
                assertParse("0x80000010", 0x80000010L);
                assertParse("-0x80000010", -0x80000010L);
                assertParse("-0x8000000000000000", -9223372036854775808L);
            }

            @Test
            void over_flow() {
                assertParse("0x8000000000000000", new BigInteger("9223372036854775808"));
                assertParse("-0x8000000000000001", new BigInteger("-9223372036854775809"));
            }

            @Test
            void invalid_number() {
                assertParse("100000000005_", null);
                assertParse("100000000005xx", null);
            }
        }

        @Nested
        class Radix2 {

            @Test
            void parse_long() {
                assertParse("0b1000_0000_0000_0000_0000_0000_0000_0000_0000", 0x800000000L);
                assertParse("-0b1000_0000_0000_0000_0000_0000_0000_0001_0000", -0x800000010L);
            }

            @Test
            void over_flow() {
                assertParse("0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000", new BigInteger("9223372036854775808"));
                assertParse("-0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001", new BigInteger("-9223372036854775809"));
            }

            @Test
            void invalid_number() {
                assertParse("0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_", null);
                assertParse("0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000b", null);
            }
        }


        @Nested
        class Radix8 {

            @Test
            void parse_long() {
                assertParse("0377777777777777", 0xfffffffffffL);
                assertParse("0377_7777_7777_7777", 0xfff_ffff_ffffL);
                assertParse("0777777777777777777777", 9223372036854775807L);
            }

            @Test
            void negative() {
                assertParse("-01000000000000000000000", -9223372036854775808L);
            }

            @Test
            void over_flow() {
                assertParse("01000000000000000000000", new BigInteger("9223372036854775808"));
                assertParse("-01000000000000000000001", new BigInteger("-9223372036854775809"));
            }

            @Test
            void invalid_number() {
                assertParse("01000000000000000000_", null);
            }
        }
    }

    @Nested
    class ParseBigInteger {

        @Nested
        class Radix10 {

            @Test
            void parse_big_int() {
                assertParse("10000000000000000005", new BigInteger("10000000000000000005"));
                assertParse("100000000000000000015", new BigInteger("100000000000000000015"));
                assertParse("100000000000000000_00_05", new BigInteger("1000000000000000000005"));
            }

            @Test
            void negative() {
                assertParse("-10000000000000000005", new BigInteger("-10000000000000000005"));
                assertParse("-1000000000000000_00_05", new BigInteger("-10000000000000000005"));
            }

            @Test
            void invalid_number() {
                assertParse("10000000000000000005_", null);
                assertParse("10000000000000000005xx", null);
            }
        }

        @Nested
        class Radix16 {

            @Test
            void parse_big_int() {
                assertParse("0x80000000000000001", new BigInteger("80000000000000001", 16));
                assertParse("0x800000000000000012", new BigInteger("800000000000000012", 16));
                assertParse("0x800000000000_000_012", new BigInteger("800000000000000012", 16));
            }

            @Test
            void negative() {
                assertParse("-0x10000000000000000005", new BigInteger("-10000000000000000005", 16));
                assertParse("-0x1000000000000000_00_05", new BigInteger("-10000000000000000005", 16));
            }

            @Test
            void invalid_number() {
                assertParse("0x10000000000000000005_", null);
                assertParse("0x10000000000000000005xx", null);
            }
        }

        @Nested
        class Radix2 {

            @Test
            void parse_big_integer() {
                assertParse("0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000", new BigInteger("80000000000000000", 16));
                assertParse("-0b1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0001_0000", new BigInteger("-80000000000000010", 16));
            }
        }

        @Nested
        class Radix8 {

            @Test
            void parse_big_int() {
                assertParse("01000000000000000000_0000", new BigInteger("9223372036854775808").multiply(BigInteger.valueOf(8)));
                assertParse("-01000000000000000000_0010", new BigInteger("-9223372036854775809").multiply(BigInteger.valueOf(8)));
            }

            @Test
            void invalid_number() {
                assertParse("01000000000000000000_0000_", null);
            }
        }
    }

    @Nested
    class ParseFloat {

        @Nested
        class FromInteger {

            @Test
            void dot_in_integer() {
                assertParse("-0.0", -0.0d);
                assertParse("1.5", 1.5d);
                assertParse("10.05", 10.05d);
                assertParse("1__0.0__5", 1__0.0__5d);
                assertParse("0.123456789", 0.123456789d);
            }

            @Test
            void invalid_double() {
                assertParse("0x1.5", null);
                assertParse("1.1_", null);
            }

            @Test
            void power_number_in_integer() {
                assertParse("-0E5", -0E5);
                assertParse("10E05", 10E5);
                assertParse("10E1_5", 10E1_5);
                assertParse("10E15", 10E15);
                assertParse("10E-5", 10E-5);
                assertParse("10E+5", 10E5);
                assertParse("0E5", 0E5);
            }

            @Test
            void invalid_power_number() {
                assertParse("10E0.5", null);
                assertParse("10E5_", null);
                assertParse("10E_5", null);
                assertParse("10E0xF", null);
                assertParse("10Ea", null);
                assertParse("e1", null);
                assertParse("-e1", null);
                assertParse("0x1E1", 0x1E1);
                assertParse("10E", null);
                assertParse("10E+", null);
                assertParse("10E-", null);
            }
        }

        @Nested
        class FromLong {

            @Test
            void dot_in_long() {
                assertParse("2147483648.5", 2147483648.5d);
                assertParse("2147483648.05", 2147483648.05d);
                assertParse("2147483648.0__5", 2147483648.0__5d);
            }

            @Test
            void dot_should_between_number() {
                assertParse("2147483648.", null);
                assertParse("2147483648.n", null);
            }

            @Test
            void invalid_double() {
                assertParse("0x2147483648.5", null);
                assertParse("2147483648.1_", null);
            }

            @Test
            void power_number_in_integer() {
                assertParse("2147483648E05", 2147483648E5);
                assertParse("2147483648E1_5", 2147483648E1_5);
                assertParse("2147483648E15", 2147483648E15);
                assertParse("2147483648E-5", 2147483648E-5);
                assertParse("2147483648E+5", 2147483648E5);

            }

            @Test
            void invalid_power_number() {
                assertParse("2147483648E0.5", null);
                assertParse("2147483648E5_", null);
                assertParse("2147483648E0xF", null);
                assertParse("2147483648EA", null);
                assertParse("0x8FFFFFFFE1", 0x8FFFFFFFE1L);
                assertParse("2147483648E", null);
            }
        }

        @Nested
        class FromBigInteger {

            @Test
            void dot_in_big_integer() {
                assertParse("100000000000000000000.5", 100000000000000000000.5d);
                assertParse("100000000000000000000.05", 100000000000000000000.05d);
                assertParse("100000000000000000000.0__5", 100000000000000000000.0__5d);
            }

            @Test
            void dot_should_between_number() {
                assertParse("100000000000000000000.", null);
                assertParse("100000000000000000015.n", null);
            }

            @Test
            void invalid_double() {
                assertParse("0x100000000000000000015.5", null);
                assertParse("100000000000000000015.1_", null);
            }

            @Test
            void power_number_in_integer() {
                assertParse("100000000000000000015E05", 100000000000000000015E5);
                assertParse("100000000000000000015E1_5", 100000000000000000015E1_5);
                assertParse("100000000000000000015E15", 100000000000000000015E15);
                assertParse("100000000000000000015E-5", 100000000000000000015E-5);
                assertParse("100000000000000000015E+5", 100000000000000000015E5);

            }

            @Test
            void invalid_power_number() {
                assertParse("100000000000000000015E0.5", null);
                assertParse("100000000000000000015E5_", null);
                assertParse("100000000000000000015E0xF", null);
                assertParse("100000000000000000015EA", null);
                assertParse("0x100000000000000000015EA", new BigInteger("100000000000000000015EA", 16));
                assertParse("100000000000000000015E", null);
            }
        }

        @Test
        void power_number_in_double() {
            assertParse("0.1E5", 0.1E5);
            assertParse("0.12E5", 0.12E5);
            assertParse("13.24E5", 13.24E5);
        }

        @Test
        void dot_should_between_number() {
            assertParse("1.", null);
            assertParse("-.5", null);
            assertParse(".5", null);
            assertParse("1.n", null);
            assertParse(".", null);
            assertParse("0.", null);
            assertParse(".0", null);
            assertParse("0.y", null);
            assertParse("0.f", null);
            assertParse("0.1", 0.1d);
            assertParse("0.9", 0.9d);
            assertParse("1.0", 1.0d);
            assertParse("9.1", 9.1d);
        }

        @Test
        void power_char_should_between_number() {
            assertParse(".E0", null);
            assertParse(".e0", null);
            assertParse("0E", null);
            assertParse("0e", null);
            assertParse(".e", null);
            assertParse("0ex", null);
            assertParse("0E.0", null);
            assertParse("0Ed", null);
        }
    }

    @Test
    void invalid_number() {
        assertParse("+", null);
        assertParse("-", null);
        assertParse("1_", null);
        assertParse("", null);
        assertParse("notNumber", null);
        assertParse("+-1", null);
    }

    @Nested
    class ParseBigDecimal {

        @Test
        void to_big_decimal_with_huge_power() {
            assertParse("100E400", new BigDecimal("100E400"));
            assertParse("-100E400", new BigDecimal("-100E400"));
        }

        @Test
        void long_float_to_big_decimal() {
            assertParse("1" + String.join("", Collections.nCopies(400, "0")) + ".0", new BigDecimal("1.0E400"));
            assertParse("-1" + String.join("", Collections.nCopies(400, "0")) + ".0", new BigDecimal("-1.0E400"));
        }
    }

    @Nested
    class Postfix {

        @Nested
        class IntegerParse {

            @Test
            void as_byte() {
                assertParse("0y", (byte) 0);
                assertParse("1y", (byte) 1);
                assertParse("-1y", (byte) -1);
                assertParse("-128y", (byte) -128);
                assertParse("127y", (byte) 127);
                assertParse("-0x80y", (byte) -128);
                assertParse("0x7fy", (byte) 127);

                assertParse("0Y", (byte) 0);
                assertParse("1Y", (byte) 1);

                assertParseOverflow("128y");
                assertParseOverflow("-129y");
            }

            @Test
            void as_short() {
                assertParse("0s", (short) 0);
                assertParse("1s", (short) 1);
                assertParse("-1s", (short) -1);
                assertParse("-0x8000s", (short) -32768);
                assertParse("0x7fffs", (short) 32767);

                assertParseOverflow("32768s");
                assertParseOverflow("-32769s");
            }

            @Test
            void as_long() {
                assertParse("0l", 0L);
                assertParse("1l", 1L);
                assertParse("-1l", -1L);
                assertParse("-0x8000_0000l", -2147483648L);
                assertParse("0x7fff_ffffl", 2147483647L);
            }

            @Test
            void as_big_integer() {
                assertParse("0bi", BigInteger.valueOf(0));
                assertParse("10bi", BigInteger.valueOf(10));
                assertParse("-10bi", BigInteger.valueOf(-10));
            }

            @Test
            void as_big_decimal() {
                assertParse("0bd", BigDecimal.valueOf(0));
                assertParse("1bd", BigDecimal.valueOf(1));
                assertParse("-1bd", BigDecimal.valueOf(-1));
            }

            @Test
            void as_float() {
                assertParse("0f", 0.0f);
                assertParse("1f", 1.0f);
                assertParse("-1f", -1.0f);
            }

            @Test
            void as_double() {
                assertParse("0d", 0.0);
                assertParse("1d", 1.0);
                assertParse("-1d", -1.0);
            }
        }

        @Nested
        class LongParse {

            @Test
            void as_byte() {
                assertParseOverflow("0xffff_ffff_ffy");
                assertParseOverflow("-0xffff_ffff_ffy");
            }

            @Test
            void as_short() {
                assertParseOverflow("0xffff_ffff_ffs");
                assertParseOverflow("-0xffff_ffff_ffs");
            }

            @Test
            void as_long() {
                assertParse("-0x8000_0001l", -2147483649L);
                assertParse("0x8000_0000l", 2147483648L);

                assertParse("-0x8000_0000_0000_0000l", 0x8000_0000_0000_0000L);
                assertParse("0x7fff_ffff_ffff_ffff", 0x7fff_ffff_ffff_ffffL);
            }

            @Test
            void as_big_integer() {
                assertParse("0xffff_ffff_ffbi", new BigInteger("ffffffffff", 16));
                assertParse("-0xffff_ffff_ffbi", new BigInteger("-ffffffffff", 16));
            }

            @Test
            void as_big_decimal() {
                assertParse("2147483648bd", BigDecimal.valueOf(2147483648L));
                assertParse("-2147483649bd", BigDecimal.valueOf(-2147483649L));
            }

            @Test
            void as_float() {
                assertParse("2147483648f", 2147483648f);
                assertParse("-2147483648f", -2147483648f);
            }

            @Test
            void as_double() {
                assertParse("2147483648d", 2147483648d);
                assertParse("-2147483648d", -2147483648d);
            }
        }


        @Nested
        class BigIntegerParser {

            @Test
            void as_byte() {
                assertParseOverflow("0x8000_0000_0000_0000y");
                assertParseOverflow("-0x8000_0000_0000_0001y");
            }

            @Test
            void as_short() {
                assertParseOverflow("0x8000_0000_0000_0000s");
                assertParseOverflow("-0x8000_0000_0000_0001s");
            }

            @Test
            void as_long() {
                assertParseOverflow("0x8000_0000_0000_0000l");
                assertParseOverflow("-0x8000_0000_0000_0001l");
            }

            @Test
            void as_big_integer() {
                assertParse("0x8000_0000_0000_0000bi", new BigInteger("8000000000000000", 16));
                assertParse("-0x8000_0000_0000_0001bi", new BigInteger("-8000000000000001", 16));
            }

            @Test
            void as_big_decimal() {
                assertParse("9223372036854775808bd", new BigDecimal("9223372036854775808"));
                assertParse("-9223372036854775809bd", new BigDecimal("-9223372036854775809"));
            }

            @Test
            void as_float() {
                assertParse("9223372036854775808f", 9223372036854775808f);
                assertParse("-9223372036854775808f", -9223372036854775808f);
            }

            @Test
            void as_double() {
                assertParse("9223372036854775808d", 9223372036854775808d);
                assertParse("-9223372036854775808d", -9223372036854775808d);
            }
        }

        @Nested
        class DotFloatParser {

            @Test
            void as_byte() {
                assertParseOverflow("0.0y");
                assertParseOverflow("2147483648.0y");
                assertParseOverflow("9223372036854775808.0y");
            }

            @Test
            void as_short() {
                assertParseOverflow("0.0s");
                assertParseOverflow("2147483648.0s");
                assertParseOverflow("9223372036854775808.0s");
            }

            @Test
            void as_long() {
                assertParseOverflow("0.0l");
                assertParseOverflow("2147483648.0l");
                assertParseOverflow("9223372036854775808.0l");
            }

            @Test
            void as_big_integer() {
                assertParseOverflow("0.0bi");
                assertParseOverflow("2147483648.0bi");
                assertParseOverflow("9223372036854775808.0bi");
            }

            @Test
            void as_big_decimal() {
                assertParse("0.0bd", new BigDecimal("0.0"));
                assertParse("2147483648.0bd", new BigDecimal("2147483648.0"));
                assertParse("9223372036854775808.0bd", new BigDecimal("9223372036854775808.0"));
            }

            @Test
            void as_float() {
                assertParse("0.0f", 0.0f);
                assertParse("2147483648.0f", 2147483648.0f);
                assertParse("9223372036854775808.0f", 9223372036854775808.0f);
            }

            @Test
            void as_double() {
                assertParse("0.0d", 0.0d);
                assertParse("2147483648.0d", 2147483648.0d);
                assertParse("9223372036854775808.0d", 9223372036854775808.0d);
            }
        }

        @Nested
        class PowerFloatParser {

            @Test
            void as_byte() {
                assertParseOverflow("0e0y");
                assertParseOverflow("2147483648e0y");
                assertParseOverflow("9223372036854775808e0y");
            }

            @Test
            void as_short() {
                assertParseOverflow("0e0s");
                assertParseOverflow("2147483648e0s");
                assertParseOverflow("9223372036854775808e0s");
            }

            @Test
            void as_long() {
                assertParseOverflow("0e0l");
                assertParseOverflow("2147483648e0l");
                assertParseOverflow("9223372036854775808e0l");
            }

            @Test
            void as_big_integer() {
                assertParseOverflow("0e0bi");
                assertParseOverflow("2147483648e0bi");
                assertParseOverflow("9223372036854775808e0bi");
            }

            @Test
            void as_big_decimal() {
                assertParse("0e0bd", new BigDecimal("0e0"));
                assertParse("2147483648e0bd", new BigDecimal("2147483648e0"));
                assertParse("9223372036854775808e0bd", new BigDecimal("9223372036854775808e0"));
            }

            @Test
            void as_float() {
                assertParse("0e0f", 0e0f);
                assertParse("2147483648e0f", 2147483648e0f);
                assertParse("9223372036854775808e0f", 9223372036854775808e0f);
            }

            @Test
            void as_double() {
                assertParse("0e0d", 0e0d);
                assertParse("2147483648e0d", 2147483648e0d);
                assertParse("9223372036854775808e0d", 9223372036854775808e0d);
            }

            @Test
            void overflow() {
                assertParseOverflow("1E200f");
                assertParseOverflow("1E400d");
            }
        }

        @Test
        void invalid_postfix_number() {
            assertParse("1_d", null);
            assertParse("1_y", null);
            assertParse("1_s", null);
            assertParse("1_L", null);
            assertParse("1_f", null);
            assertParse("1_bd", null);
            assertParse("1_bi", null);
        }
    }

    private void assertParseOverflow(String code) {
        assertThat(assertThrows(NumberOverflowException.class, () -> new NumberParser().parse(code)))
                .hasMessageContaining(String.format("Cannon save [%s] with the given postfix type", code));
    }

    private void assertParse(String inputCode, Number expected) {
        if (expected instanceof BigDecimal) {
            assertThat(((BigDecimal) new NumberParser().parse(inputCode)).subtract((BigDecimal) expected)).isZero();
        } else
            assertThat(new NumberParser().parse(inputCode)).isEqualTo(expected);
    }
}
