package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class OpeningBraceTokenFactoryTest extends TokenFactoryTestBase {

    @Override
    protected TokenFactory createTokenFactory() {
        return TokenFactory.createOpeningBraceTokenFactory();
    }

    @Override
    protected Token createToken(Object value) {
        return Token.openingBraceToken();
    }

    @Test
    void return_empty_when_does_not_match_code() {
        assertThat(parseToken("(")).isNull();
    }

    @Test
    void return_token_when_matches_code() {
        assertThat(parseToken("{")).isEqualTo(Token.openingBraceToken());
    }

    @Test
    void should_take_char_when_matches_code() {
        assertThat(nextCharOf("{1")).isEqualTo('1');
    }
}
