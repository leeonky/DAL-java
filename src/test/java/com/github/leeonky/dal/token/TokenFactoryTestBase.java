package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public abstract class TokenFactoryTestBase {
    protected abstract TokenFactory createTokenFactory();

    protected void shouldParse(String code, String value) {
        assertThat(parseToken(code)).isEqualTo(createToken(value));
    }

    protected abstract Token createToken(String value);

    protected Token parseToken(String s) {
        return createTokenFactory().fetchToken(new SourceCode(s), previousToken());
    }

    protected Token previousToken() {
        return null;
    }

    protected SyntaxException invalidSyntaxCode(String s) {
        return assertThrows(SyntaxException.class, () -> createTokenFactory().fetchToken(new SourceCode(s), previousToken()));
    }
}
