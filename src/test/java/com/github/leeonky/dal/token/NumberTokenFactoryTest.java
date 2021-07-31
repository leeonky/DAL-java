package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class NumberTokenFactoryTest {
    private final TokenFactory tokenFactory = new NumberTokenFactory();

    @Test
    void return_empty_when_no_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(""), null))
                .isNull();
    }

    @Test
    void return_empty_when_first_char_is_not_digital() {
        assertThat(tokenFactory.fetchToken(new SourceCode("not start with digital"), null))
                .isNull();
    }

    @Test
    void should_return_number_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode("100"), null))
                .isEqualTo(Token.constValueToken(new BigDecimal("100")));
    }

    @Test
    void support_hex_digits() {
        assertThat(tokenFactory.fetchToken(new SourceCode("0x100"), null))
                .isEqualTo(Token.constValueToken(new BigDecimal("256")));
    }

    @Test
    void support_double_to_big_decimal() {
        assertThat(tokenFactory.fetchToken(new SourceCode("0.123"), null))
                .isEqualTo(Token.constValueToken(new BigDecimal("0.123")));
    }

    @Test
    void should_rollback_last_end_char_in_source_code() {
        SourceCode sourceCode = new SourceCode("1=");
        tokenFactory.fetchToken(sourceCode, null);

        assertThat(sourceCode.getChar()).isEqualTo('=');
    }
}
