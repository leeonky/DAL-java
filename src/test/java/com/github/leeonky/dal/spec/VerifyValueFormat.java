package com.github.leeonky.dal.spec;

import com.github.leeonky.dal.format.Formatters;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.HashMap;

class VerifyValueFormat extends Base {

    public enum E {
        A, b
    }

    public static class EnumSchema {
        public Formatters.Enum<E> e;
    }

    @Nested
    class VerifyNumber {

        @Test
        void should_check_number() {
            assertPass(null, "1 is PositiveInteger");
            assertFailed(null, "'1' is PositiveInteger");
            assertFailed(null, "1.0 is PositiveInteger");
            assertFailed(null, "0 is PositiveInteger");
            assertFailed(null, "(-1) is PositiveInteger");

            assertPass(null, "10 is Integer");
            assertPass(null, "1000000000000000000 is Integer");
            assertPass(null, "10000000000000000000000000000 is Integer");

            assertFailed((float) 1.0, "is Integer");
            assertFailed((float) 0.1, "is Integer");
            assertFailed(null, "0.000000000000000000000000001 is Integer");
            assertPass(null, "(-10000000000000000000000000000) is Integer");
        }
    }

    @Nested
    class VerifyStringValue {

        @Test
        void verify_string_type() {
            assertPass("", "is String");
        }

        @Test
        void verify_url() {
            assertPass("http://www.baidu.com", "is URL");
            assertPass("http://www.baidu.com", "is URL which .protocol = 'http' and .host = 'www.baidu.com'");
        }

        @Test
        void verify_instant() {
            assertPass("2001-12-10T10:00:11Z", "is Instant");
            assertFailed("2001-12-10T10:00:11", "is Instant");
        }
    }

    @Nested
    class VerifyEnumInSchema {
        @BeforeEach
        void registerSchema() {
            dataAssert.getRuntimeContextBuilder().registerSchema(EnumSchema.class);
        }

        @Test
        void just_check_upper_case_string_in_DAL_code() {
            assertPass("E1", "is Enum");
            assertFailed("e1", "is Enum");
        }

        @Test
        void support_check_enum_value_in_schema_verification() {
            assertPass(new HashMap<String, String>() {{
                put("e", "A");
            }}, "is EnumSchema");

            assertPass(new HashMap<String, String>() {{
                put("e", "b");
            }}, "is EnumSchema");

            assertFailed(new HashMap<String, String>() {{
                put("e", "x");
            }}, "is EnumSchema");
        }
    }
}
