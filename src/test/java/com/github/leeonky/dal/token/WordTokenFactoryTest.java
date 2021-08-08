package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;

class WordTokenFactoryTest {

    private void assertToken(String code, Token token) {
        assertThat(TokenFactory.createWordTokenFactory().fetchToken(new ParsingContext(new SourceCode(code), null)))
                .isEqualTo(token);
    }

    private void assertWordToken(String hello, String o) {
        assertToken(hello, wordToken(o));
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
            assertToken("and ", operatorToken("&&"));
            assertToken("or ", operatorToken("||"));
        }

        @Test
        void other_word_token() {
            assertWordToken("hello ", "hello");
            assertWordToken("order.product ", "order.product");
            assertWordToken("_001 ", "_001");
            assertWordToken("order_1 ", "order_1");
        }
    }

    @Nested
    class HasDelimiter {
        @ParameterizedTest
        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ' ', ':', '\t', '\n'})
        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
            TokenFactory tokenFactory = TokenFactory.createWordTokenFactory();
            SourceCode sourceCode = new SourceCode("Order.Detail" + c);
            assertThat(tokenFactory.fetchToken(new ParsingContext(sourceCode, null)))
                    .isEqualTo(wordToken("Order.Detail"));
            assertThat(sourceCode.currentChar()).isEqualTo(c);
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void allow_get_value_when_parser_not_finished() {
            assertWordToken("order", "order");
        }
    }
}