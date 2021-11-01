package com.github.leeonky.dal.compiler;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class SourceCodeTest {

    @Nested
    class EndOfLine {

        @Test
        void at_end_of_code() {
            assertThat(new SourceCode("").isEndOfLine()).isTrue();
        }

        @Test
        void space_before_change_line() {
            assertThat(new SourceCode("\n").isEndOfLine()).isTrue();
            assertThat(new SourceCode(" \n").isEndOfLine()).isTrue();
            assertThat(new SourceCode("\t\n").isEndOfLine()).isTrue();
            assertThat(new SourceCode(" \t\r\n").isEndOfLine()).isTrue();
        }
    }
}