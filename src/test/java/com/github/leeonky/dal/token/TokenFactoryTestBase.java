package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.parser.TokenParser;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public abstract class TokenFactoryTestBase {
    protected abstract TokenFactory createTokenFactory();

    protected void shouldParse(String code, String value) {
        assertThat(parseToken(code)).isEqualTo(createToken(value));
    }

    protected abstract Token createToken(String value);

    protected Token parseToken(String sourceCode) {
        return parseToken(sourceCode, previousToken());
    }

    protected Token parseToken(String sourceCode, Token last) {
        TokenStream tokenStream = new TokenStream();
        if (last != null)
            tokenStream.appendToken(last);
        return createTokenFactory().fetchToken(new TokenParser(new SourceCode(sourceCode), tokenStream));
    }

    protected Token previousToken() {
        return null;
    }

    protected SyntaxException invalidSyntaxCode(String s) {
        return assertThrows(SyntaxException.class, () -> parseToken(s, previousToken()));
    }
}
