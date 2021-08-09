package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.math.BigDecimal;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static org.assertj.core.api.Assertions.assertThat;

class NumberTokenFactoryTest extends TokenFactoryTestBase {

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createNumberTokenFactory();
    }

    @Override
    protected Token createToken(Object value) {
        return constValueToken(new BigDecimal((String) value));
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with digital")).isNull();
        }

        @Test
        void return_empty_when_invalid_number() {
            assertThat(parseToken("1ab ")).isNull();
        }

        @Test
        void should_return_code_position_when_parse_invalid_number() {
            assertThat(nextCharOf("1ab ")).isEqualTo('1');
        }
    }

    @Nested
    class Parse {

        @Test
        void should_return_token_when_given_valid_code() {
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
    }

    @Nested
    class HasDelimiter {

        @ParameterizedTest
        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ' ', ':', '\t', '\n'})
        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
            assertThat(nextCharOf("100" + c)).isEqualTo(c);
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

