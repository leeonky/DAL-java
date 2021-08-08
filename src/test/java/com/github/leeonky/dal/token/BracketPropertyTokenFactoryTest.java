package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Scanner.OPT_MATCHES_STRING;
import static org.assertj.core.api.Assertions.assertThat;

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
    }

    @Nested
    class Parse {

        @Test
        void support_array_access() {
            assertThat(parseToken("[1]")).isEqualTo(Token.propertyToken(1));
        }

        @Test
        void invalid_number_for_array_access() {
            assertThat(invalidSyntaxCode("[1.1]"))
                    .hasMessage("must be integer")
                    .hasFieldOrPropertyWithValue("position", 1);
        }
        //TODO invalid number
    }

    @Nested
    class HasDelimiter {

//        @Test
//        void should_return_token_when_given_valid_code() {
//            shouldParse(".a ", "a");
//        }
//
//        @ParameterizedTest
//        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ':', ' ', '\t', '\n'})
//        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
//            TokenFactory tokenFactory = createTokenFactory();
//            SourceCode sourceCode = new SourceCode(".a" + c);
//            assertThat(tokenFactory.fetchToken(new ParsingContext(sourceCode, null)))
//                    .isEqualTo(propertyToken("a"));
//            assertThat(sourceCode.currentChar()).isEqualTo(c);
//        }
//
//        @Test
//        void do_not_allow_get_value_when_no_value() {
//            assertThat(assertThrows(SyntaxException.class, () -> parseToken(". ")))
//                    .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 2);
//        }
    }

    @Nested
    class NoDelimiter {

//        @Test
//        void allow_get_value_when_parser_not_finished() {
//            shouldParse(".a", "a");
//        }
//
//        @Test
//        void do_not_allow_get_value_when_no_value() {
//            assertThat(assertThrows(SyntaxException.class, () -> parseToken(".")))
//                    .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 1);
//        }
    }
}
