package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.propertyToken;
import static org.assertj.core.api.Assertions.assertThat;

class BracketPropertyTokenFactoryTest {
    private TokenFactory createTokenFactory() {
        return TokenFactory.createBracketPropertyTokenFactory();
    }

    private void shouldParse(String code, String value) {
        assertThat(parseToken(code)).isEqualTo(propertyToken(value));
    }

    private Token parseToken(String s) {
        final SourceCode sourceCode = new SourceCode(s);
        return createTokenFactory().fetchToken(new ParsingContext(sourceCode, null));
    }

    @Nested
    class CodeMatches {

        @Test
        void return_empty_when_first_char_is_not_digital() {
            assertThat(parseToken("not start with .")).isNull();
        }
    }
}
