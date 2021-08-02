package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static org.assertj.core.api.Assertions.assertThat;

class NumberTokenFactoryTest {
    private TokenFactory createTokenFactory() {
        return new NumberTokenFactory();
    }

    private void shouldParse(String code, String value) {
        assertThat(parseToken(code)).isEqualTo(constValueToken(new BigDecimal(value)));
    }

    private Token parseToken(String s) {
        return createTokenFactory().fetchToken(new SourceCode(s), null);
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_no_code() {
            assertThat(parseToken("")).isNull();
        }

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with digital")).isNull();
        }
    }

    @Nested
    class HasDelimiter {

        @Test
        void should_return_number_token_when_given_valid_code() {
            shouldParse("100 ", "100");
        }

        @Test
        void support_hex_digits() {
            shouldParse("0x100 ", "256");
        }

        @Test
        void support_double_to_big_decimal() {
            shouldParse("0.123 ", "0.123");
        }

        @ParameterizedTest
        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ' ', '\t', '\n'})
        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
            TokenFactory tokenFactory = createTokenFactory();
            SourceCode sourceCode = new SourceCode("100" + c);
            assertThat(tokenFactory.fetchToken(sourceCode, null))
                    .isEqualTo(constValueToken(new BigDecimal("100")));
            assertThat(sourceCode.getChar()).isEqualTo(c);
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void allow_get_value_when_parser_not_finished() {
            shouldParse("0.123", "0.123");
        }
    }
}

