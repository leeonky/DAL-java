package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.TokenContentInString.ALL_CHARACTERS;
import static com.github.leeonky.dal.parser.TokenContentInString.leftTrim;
import static org.assertj.core.api.Assertions.assertThat;

class TokenContentInStringTest {

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
            TokenContentInString taker = ALL_CHARACTERS
                    .escape("ab", 'x')
                    .escape("cd", 'y');

            assertThat(taker.getChar(sourceCode)).isEqualTo('x');
            assertThat(taker.getChar(sourceCode)).isEqualTo('=');
            assertThat(taker.getChar(sourceCode)).isEqualTo('y');
        }

        @Test
        void copied_getter_with_the_same_behavior() {
            SourceCode sourceCode = new SourceCode("ab=cd");
            TokenContentInString taker = ALL_CHARACTERS
                    .escape("ab", 'x')
                    .escape("cd", 'y').copy();

            assertThat(taker.getChar(sourceCode)).isEqualTo('x');
            assertThat(taker.getChar(sourceCode)).isEqualTo('=');
            assertThat(taker.getChar(sourceCode)).isEqualTo('y');
        }
    }

    @Nested
    class LeftTrim {
        TokenContentInString content = leftTrim(ALL_CHARACTERS);

        @Test
        void left_trim_white_space() {
            SourceCode sourceCode = new SourceCode("  a");

            content.preprocess(sourceCode);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }

        @Test
        void copied_getter_with_the_same_behavior() {
            SourceCode sourceCode = new SourceCode("  a");

            content.copy().preprocess(sourceCode);

            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }
    }
}
