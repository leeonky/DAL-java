package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Scanner.OPT_MATCHES_STRING;
import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class BracketPropertyTokenFactoryTest extends TokenFactoryTestBase {

    public static final Token OPT_MATCHES = Token.operatorToken(OPT_MATCHES_STRING);

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createBracketPropertyTokenFactory();
    }

    @Override
    protected Token createToken(String value) {
        try {
            return Token.propertyToken(Integer.valueOf(value));
        } catch (NumberFormatException e) {
            return Token.propertyToken(value);
        }
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_bracket() {
            assertThat(parseToken("not [")).isNull();
        }

        @Test
        void return_empty_when_bracket_after_matches() {
            SourceCode sourceCode = new SourceCode("[");
            assertThat(createTokenFactory().fetchToken(new ParsingContext(sourceCode, OPT_MATCHES)))
                    .isNull();
        }

        @Test
        void should_raise_error_when_unexpected_token() {
            assertThat(invalidSyntaxCode("[+]"))
                    .hasMessage("Unexpected token")
                    .hasFieldOrPropertyWithValue("position", 1);
        }
    }

    @Nested
    class Parse {

        @Nested
        class Array {

            @Test
            void support_array_access() {
                assertThat(parseToken("[1]")).isEqualTo(Token.propertyToken(1));
            }

            @Test
            void index_of_array_access_must_be_int() {
                assertThat(invalidSyntaxCode("[1.1]"))
                        .hasMessage("must be integer")
                        .hasFieldOrPropertyWithValue("position", 1);
            }
        }

        @Nested
        class SingleQuotedString {

            @Test
            void support_array_access() {
                assertThat(parseToken("['key']")).isEqualTo(Token.propertyToken("key"));
            }
        }

        @Nested
        class DoubleQuotedString {

            @Test
            void support_array_access() {
                assertThat(parseToken("[\"key\"]")).isEqualTo(Token.propertyToken("key"));
            }
        }
    }

    @Nested
    class HasDelimiter {

        @Test
        void seek_to_right_position_after_fetch_token() {
            SourceCode sourceCode = new SourceCode("[0]=");

            createTokenFactory().fetchToken(new ParsingContext(sourceCode, null));

            assertThat(sourceCode.currentChar()).isEqualTo('=');
        }

        @Test
        void do_not_allow_get_value_when_no_value() {
            assertThat(assertThrows(SyntaxException.class, () -> parseToken("[]")))
                    .hasMessage("should given one property or array index in `[]`").hasFieldOrPropertyWithValue("position", 1);
        }

        @Test
        void do_not_allow_more_than_one_sub_token() {
            assertThat(assertThrows(SyntaxException.class, () -> parseToken("[1 2]")))
                    .hasMessage("should given one property or array index in `[]`").hasFieldOrPropertyWithValue("position", 4);
        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void do_not_allow_get_value_when_no_value() {
            assertThat(assertThrows(SyntaxException.class, () -> parseToken("[1")))
                    .hasMessage("should end with `]`").hasFieldOrPropertyWithValue("position", 2);
        }
    }
}
