package com.github.leeonky.dal;

import com.github.leeonky.dal.compiler.SyntaxException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DalExceptionTest {

    @Nested
    class Show {

        @Test
        void mark_one_line_code() {
            assertThat(new SyntaxException("", 0).show("12345"))
                    .isEqualTo("" +
                            "12345\n" +
                            "^");

            assertThat(new SyntaxException("", 4).show("12345"))
                    .isEqualTo("" +
                            "12345\n" +
                            "    ^");
        }

        @Test
        void mark_several_lines_code() {
            assertThat(new SyntaxException("", 6).show("" +
                    "12345\n" +
                    "abcde"))
                    .isEqualTo("" +
                            "12345\n" +
                            "abcde\n" +
                            "^");

            assertThat(new SyntaxException("", 10).show("" +
                    "12345\n" +
                    "abcde"))
                    .isEqualTo("" +
                            "12345\n" +
                            "abcde\n" +
                            "    ^");
        }
    }
}
