package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DoubleQuotationStringTokenFactoryTest {
    private final TokenFactory tokenFactory = new DoubleQuotationStringTokenFactory();

    @Test
    void return_empty_when_no_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(""), null))
                .isNull();
    }

    @Test
    void return_empty_when_not_match_single_quotation_string() {
        assertThat(tokenFactory.fetchToken(new SourceCode("not start with `\"`"), null))
                .isNull();
    }

    @Test
    void should_return_string_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode("\"hello\""), null))
                .isEqualTo(Token.constValueToken("hello"));
    }

    @Test
    void seek_to_right_position_after_fetch_token() {
        SourceCode sourceCode = new SourceCode("\"hello\"=");

        tokenFactory.fetchToken(sourceCode, null);

        assertThat(sourceCode.currentChar()).isEqualTo('=');
    }

    @Test
    void raise_error_when_string_not_complete() {
        assertThat(assertThrows(SyntaxException.class, () -> tokenFactory.fetchToken(new SourceCode("\"hello"), null)))
                .hasMessage("string should end with `\"`").hasFieldOrPropertyWithValue("position", 6);
    }
}