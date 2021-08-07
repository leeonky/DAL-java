package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static com.github.leeonky.dal.token.Scanner.OPT_MATCHES_STRING;
import static com.github.leeonky.dal.token.Token.operatorToken;
import static org.assertj.core.api.Assertions.assertThat;

class OperatorTokenFactoryTest extends TokenFactoryTestBase {

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createOperatorTokenFactory();
    }

    @Override
    protected Token createToken(String value) {
        return Token.operatorToken(value);
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with operator char")).isNull();
        }

        @Test
        void should_return_empty_when_last_token_is_operator_matches() {
            final SourceCode sourceCode = new SourceCode("/");
            assertThat(createTokenFactory().fetchToken(new ParsingContext(sourceCode, Token.operatorToken(OPT_MATCHES_STRING))))
                    .isNull();

            final SourceCode sourceCode1 = new SourceCode("+");
            assertThat(createTokenFactory().fetchToken(new ParsingContext(sourceCode1, Token.operatorToken(OPT_MATCHES_STRING))))
                    .isEqualTo(Token.operatorToken("+"));

            final SourceCode sourceCode2 = new SourceCode("/");
            assertThat(createTokenFactory().fetchToken(new ParsingContext(sourceCode2, null)))
                    .isEqualTo(Token.operatorToken("/"));
        }

    }

    @Nested
    class HasDelimiter {

        @Test
        void should_return_token_when_given_valid_code() {
            shouldParse("+ ", "+");
        }

        @Test
        void distinguish_regex_after_operator_matches() {
            shouldParse(":/ ", ":");
        }

        @ParameterizedTest
        @ValueSource(strings = {"-", "!", "=", ">", "<", "+", "*", "/", ":", ">=", "<=", "!=", "&&", "||"})
        void finish_parse_and_source_code_seek_back_to_delimiter(String opt) {
            TokenFactory tokenFactory = createTokenFactory();
            SourceCode sourceCode = new SourceCode(opt + "a");
            assertThat(tokenFactory.fetchToken(new ParsingContext(sourceCode, null)))
                    .isEqualTo(operatorToken(opt));
            assertThat(sourceCode.currentChar()).isEqualTo('a');
        }
    }

    @Nested
    class NoDelimiter {

        @ParameterizedTest
        @ValueSource(strings = {"-", "!", "=", ">", "<", "+", "*", "/", ":", ">=", "<=", "!=", "&&", "||"})
        void allow_get_value_when_parser_not_finished(String opt) {
            shouldParse(opt, opt);
        }
    }
}
