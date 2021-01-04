package com.github.leeonky.dal;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DalExceptionTest {

    @Nested
    class Show {

        @Test
        void mark_one_line_code() {
            assertThat(new DalException("", 0).show("12345"))
                    .isEqualTo("" +
                            "12345\n" +
                            "^");

            assertThat(new DalException("", 4).show("12345"))
                    .isEqualTo("" +
                            "12345\n" +
                            "    ^");
        }

        @Test
        void mark_several_lines_code() {
            assertThat(new DalException("", 6).show("" +
                    "12345\n" +
                    "abcde"))
                    .isEqualTo("" +
                            "12345\n" +
                            "abcde\n" +
                            "^");

            assertThat(new DalException("", 10).show("" +
                    "12345\n" +
                    "abcde"))
                    .isEqualTo("" +
                            "12345\n" +
                            "abcde\n" +
                            "    ^");
        }
    }
}
