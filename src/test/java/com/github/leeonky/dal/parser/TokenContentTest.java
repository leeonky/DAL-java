package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.TokenContent.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenContent.leftTrim;
import static org.assertj.core.api.Assertions.assertThat;

class TokenContentTest {

    @Nested
    class GetChar {

        @Test
        void get_char_one_char_from_source_code() {
            SourceCode sourceCode = new SourceCode("ab");
            assertThat(ALL_CHARACTERS.getChar(sourceCode)).isEqualTo('a');
            assertThat(ALL_CHARACTERS.getChar(sourceCode)).isEqualTo('b');

        }

        @Test
        void support_escape() {
            SourceCode sourceCode = new SourceCode("ab=cd");
            TokenContent taker = ALL_CHARACTERS
                    .escape("ab", 'x')
                    .escape("cd", 'y');

            assertThat(taker.getChar(sourceCode)).isEqualTo('x');
            assertThat(taker.getChar(sourceCode)).isEqualTo('=');
            assertThat(taker.getChar(sourceCode)).isEqualTo('y');
        }

        @Test
        void copied_getter_with_the_same_behavior() {
            SourceCode sourceCode = new SourceCode("ab=cd");
            TokenContent taker = ALL_CHARACTERS
                    .escape("ab", 'x')
                    .escape("cd", 'y').copy();

            assertThat(taker.getChar(sourceCode)).isEqualTo('x');
            assertThat(taker.getChar(sourceCode)).isEqualTo('=');
            assertThat(taker.getChar(sourceCode)).isEqualTo('y');
        }
    }

    @Nested
    class LeftTrim {
        TokenContent taker = leftTrim(ALL_CHARACTERS);

        @Test
        void left_trim_white_space() {
            SourceCode sourceCode = new SourceCode("  a");

            taker.preprocess(sourceCode);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }

        @Test
        void copied_getter_with_the_same_behavior() {
            SourceCode sourceCode = new SourceCode("  a");

            taker.copy().preprocess(sourceCode);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }
    }
}
