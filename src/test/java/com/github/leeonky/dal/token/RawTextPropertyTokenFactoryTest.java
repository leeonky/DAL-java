package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RawTextPropertyTokenFactoryTest {
    private TokenFactory createTokenFactory() {
        return TokenFactory.createRawTextPropertyTokenFactory();
    }

    private void shouldParse(String code, Token token) {
        assertThat(parseToken(code)).isEqualTo(token);
    }

    private Token parseToken(String s) {
        return createTokenFactory().fetchToken(new SourceCode(s), null);
    }

    @Nested
    class HasDelimiter {

        @Nested
        class ListElementIndex {

            @Test
            void should_return_list_element_index_token_when_given_a_number_index() {
                shouldParse("1]", Token.constIndexToken(1));
            }
        }

        @Nested
        class RawText {

            @Test
            void should_return_property_token_when_given_a_raw_text() {
                shouldParse("]", Token.propertyToken(""));
            }
        }


//        @ParameterizedTest
//        @ValueSource(chars = {'(', ')', '=', '>', '<', '+', '-', '*', '/', '&', '|', '!', '[', ']', ' ', '\t', '\n'})
//        void finish_parse_and_source_code_seek_back_to_delimiter(char c) {
//            TokenFactory tokenFactory = createTokenFactory();
//            SourceCode sourceCode = new SourceCode(".a" + c);
//            assertThat(tokenFactory.fetchToken(sourceCode, null))
//                    .isEqualTo(propertyToken("a"));
//            assertThat(sourceCode.getChar()).isEqualTo(c);
//        }
    }

    @Nested
    class NoDelimiter {

        @Test
        void do_not_allow_get_value_when_no_end_char() {
            assertThat(assertThrows(SyntaxException.class, () -> parseToken("not ends")))
                    .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 8);
        }
    }
}
