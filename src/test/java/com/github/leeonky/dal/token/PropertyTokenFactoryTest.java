package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.github.leeonky.dal.token.Token.propertyToken;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PropertyTokenFactoryTest {
    private TokenFactory createTokenFactory() {
        return TokenFactory.createBeanPropertyTokenFactory();
    }

    private void shouldParse(String code, String... values) {
        assertThat(parseToken(code)).isEqualTo(propertyToken(values));
    }

    private Token parseToken(String s) {
        return createTokenFactory().fetchToken(new SourceCode(s), null);
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with .")).isNull();
        }
    }

    @Nested
    class Parse {

        @Test
        void support_skip_white_space() {
            shouldParse(". a ", "a");
            shouldParse(".\na ", "a");
            shouldParse(".\ta ", "a");
        }
    }

    @Nested
    class HasDelimiter {

        @Test
        void should_return_token_when_given_valid_code() {
            shouldParse(".a ", "a");
            shouldParse(".a.b ", "a", "b");
        }

        @ParameterizedTest
        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ' ', '\t', '\n'})
        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
            TokenFactory tokenFactory = createTokenFactory();
            SourceCode sourceCode = new SourceCode(".a" + c);
            assertThat(tokenFactory.fetchToken(sourceCode, null))
                    .isEqualTo(propertyToken("a"));
            assertThat(sourceCode.currentChar()).isEqualTo(c);
        }

        @Test
        void do_not_allow_get_value_when_no_value() {
            assertThat(assertThrows(SyntaxException.class, () -> parseToken(". ")))
                    .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 2);
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void allow_get_value_when_parser_not_finished() {
            shouldParse(".a", "a");
        }

        @Test
        void do_not_allow_get_value_when_no_value() {
            assertThat(assertThrows(SyntaxException.class, () -> parseToken(".")))
                    .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 1);
        }
    }
}

