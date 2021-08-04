package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class SingleQuotedStringTokenFactoryTest {
    private TokenFactory createTokenFactory() {
        return TokenFactory.createSingleQuotedStringTokenFactory();
    }

    private void shouldParse(String code, String value) {
        assertThat(parseToken(code)).isEqualTo(constValueToken(value));
    }

    private Token parseToken(String s) {
        return createTokenFactory().fetchToken(new SourceCode(s), null);
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with single quoted")).isNull();
        }
    }

    @Nested
    class Escape {
        @Test
        void escape_char() {
            shouldParse("'\\''", "'");
            shouldParse("'\\\\'", "\\");
        }

        @Test
        void keep_origin_when_not_supported_escape_char() {
            shouldParse("'\\a'", "\\a");
        }
    }

    @Nested
    class HasDelimiter {

        @Test
        void should_return_token_when_given_valid_code() {
            shouldParse("'hello'", "hello");
        }

        @Test
        void seek_to_right_position_after_fetch_token() {
            SourceCode sourceCode = new SourceCode("'hello'=");

            createTokenFactory().fetchToken(sourceCode, null);

            assertThat(sourceCode.currentChar()).isEqualTo('=');
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void allow_get_value_when_parser_not_finished() {
            assertThat(assertThrows(SyntaxException.class, () -> createTokenFactory().fetchToken(new SourceCode("'no end"), null)))
                    .hasMessage("string should end with `'`").hasFieldOrPropertyWithValue("position", 7);

            assertThat(assertThrows(SyntaxException.class, () -> createTokenFactory().fetchToken(new SourceCode("'escape is not complete\\"), null)))
                    .hasMessage("string should end with `'`").hasFieldOrPropertyWithValue("position", 24);
        }
    }
}
