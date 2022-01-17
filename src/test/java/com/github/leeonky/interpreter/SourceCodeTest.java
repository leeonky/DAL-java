package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class SourceCodeTest {

    @Nested
    class HasCode {

        @Test
        void should_has_code_when_position_not_end() {
            assertThat(new SourceCode("a").hasCode()).isTrue();
            assertThat(new SourceCode(" ").hasCode()).isTrue();
        }

        @Test
        void should_not_has_code_when_at_the_end() {
            assertThat(new SourceCode("").hasCode()).isFalse();
        }

        @Test
        void check_has_code_after_pop() {
            SourceCode code = new SourceCode("ab ");
            code.popChar();
            assertThat(code.hasCode()).isTrue();
            code.popChar();
            assertThat(code.hasCode()).isTrue();
            code.popChar();
            assertThat(code.hasCode()).isFalse();
        }
    }

    @Nested
    class StartsWith {

        @Test
        void start_with_given_word() {
            assertThat(new SourceCode("ab").startsWith("a")).isTrue();
        }

        @Test
        void not_start_with_given_word() {
            assertThat(new SourceCode("ab").startsWith("b")).isFalse();
        }

        @Test
        void trim_blank_and_start_with_given_word() {
            assertThat(new SourceCode(" \n\r\tab").startsWith("a")).isTrue();
        }
    }

    @Nested
    class EscapedPop {
       
    }

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