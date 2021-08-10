package com.github.leeonky.dal.token;

import com.github.leeonky.dal.Constants;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

class OperatorTokenFactoryTest extends TokenFactoryTestBase {

    private static final Token OPT_MATCHES = Token.operatorToken(Constants.Operators.MATCH);
    private static final Token OPT_EQ = Token.operatorToken(Constants.Operators.EQ);

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createOperatorTokenFactory();
    }

    @Override
    protected Token createToken(Object value) {
        return Token.operatorToken((String) value);
    }

    @Nested
    class CodeMatches {


        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with operator char")).isNull();
        }

        @Test
        void should_return_empty_when_last_token_is_operator_matches_or_eq() {
            assertThat(parseToken("/", OPT_MATCHES))
                    .isNull();

            assertThat(parseToken("/", OPT_EQ))
                    .isNull();

            assertThat(parseToken("/", null))
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
        void distinguish_regex_after_operator_judgement() {
            shouldParse(":/ ", ":");
            shouldParse("=/ ", "=");
        }

        @ParameterizedTest
        @ValueSource(strings = {"-", "!", "=", ">", "<", "+", "*", "/", ":", ">=", "<=", "!=", "&&", "||"})
        void finish_parse_and_source_code_seek_back_to_delimiter(String opt) {
            assertThat(nextCharOf(opt + "a")).isEqualTo('a');
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
