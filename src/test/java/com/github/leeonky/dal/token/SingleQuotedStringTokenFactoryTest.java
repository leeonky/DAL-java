package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static org.assertj.core.api.Assertions.assertThat;

class SingleQuotedStringTokenFactoryTest extends TokenFactoryTestBase {

    @Override
    protected Token createToken(String value) {
        return constValueToken(value);
    }

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createSingleQuotedStringTokenFactory();
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with single quoted")).isNull();
        }
    }

    @Nested
    class Parse {

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
            shouldParse("''", "");
            shouldParse("'hello'", "hello");
        }

        @Test
        void seek_to_right_position_after_fetch_token() {
            SourceCode sourceCode = new SourceCode("'hello'=");

            createTokenFactory().fetchToken(new ParsingContext(sourceCode, null));

            assertThat(sourceCode.currentChar()).isEqualTo('=');
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void allow_get_value_when_parser_not_finished() {
            assertThat(invalidSyntaxCode("'no end"))
                    .hasMessage("string should end with `'`")
                    .hasFieldOrPropertyWithValue("position", 7);

            assertThat(invalidSyntaxCode("'escape is not complete\\"))
                    .hasMessage("string should end with `'`")
                    .hasFieldOrPropertyWithValue("position", 24);
        }
    }
}
