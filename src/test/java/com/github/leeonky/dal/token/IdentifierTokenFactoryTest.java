package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.TokenParser;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;

class IdentifierTokenFactoryTest extends TokenFactoryTestBase {

    private void assertToken(String code, Token token) {
        assertThat(TokenFactory.createIdentifierTokenFactory().fetchToken(new TokenParser(new SourceCode(code))))
                .isEqualTo(token);
    }

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createIdentifierTokenFactory();
    }

    @Override
    protected Token createToken(Object value) {
        return Token.identifierToken(value);
    }

    @Nested
    class Parse {

        @Test
        void should_key_word_token() {
            assertToken("null ", constValueToken(null));
            assertToken("is ", keyWordToken("is"));
            assertToken("which ", keyWordToken("which"));
            assertToken("true ", constValueToken(true));
            assertToken("false ", constValueToken(false));
            assertToken("and ", operatorToken("and"));
            assertToken("or ", operatorToken("or"));
        }

        @Test
        void other_identifier_token() {
            shouldParse("hello ", "hello");
            shouldParse("order.product ", "order.product");
            shouldParse("_001 ", "_001");
            shouldParse("order_1 ", "order_1");
        }
    }

    @Nested
    class HasDelimiter {
        @ParameterizedTest
        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', ',', '[', ']', ' ', ':', '\t', '\n'})
        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
            assertThat(nextCharOf("Order.Detail" + c)).isEqualTo(c);
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void allow_get_value_when_parser_not_finished() {
            shouldParse("order", "order");
        }
    }
}