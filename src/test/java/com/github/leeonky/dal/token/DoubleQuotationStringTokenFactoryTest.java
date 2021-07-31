package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DoubleQuotationStringTokenFactoryTest {
    private final TokenFactory tokenFactory = new DoubleQuotationStringTokenFactory();

    @Test
    void return_empty_when_no_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode("")))
                .isNull();
    }

    @Test
    void return_empty_when_not_match_single_quotation_string() {
        assertThat(tokenFactory.fetchToken(new SourceCode("not start with `\"`")))
                .isNull();
    }

    @Test
    void should_return_string_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode("\"hello\"")))
                .isEqualTo(Token.constValueToken("hello"));
    }

    @Test
    void raise_error_when_string_not_complet() {
        assertThat(assertThrows(SyntaxException.class, () -> tokenFactory.fetchToken(new SourceCode("\"hello"))))
                .hasMessage("string should end with `\"`").hasFieldOrPropertyWithValue("position", 6);
    }
}