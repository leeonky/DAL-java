package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import com.github.leeonky.dal.parser.TokenParser;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

public abstract class TokenFactoryTestBase {
    protected abstract TokenFactory createTokenFactory();

    protected void shouldParse(String code, Object value) {
        assertThat(parseToken(code)).isEqualTo(createToken(value));
    }

    protected abstract Token createToken(Object value);

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

    protected char nextCharOf(String code) {
        SourceCode sourceCode = new SourceCode(code);
        TokenStream tokenStream = new TokenStream();
        Token token = previousToken();
        if (token != null)
            tokenStream.appendToken(token);
        createTokenFactory().fetchToken(new TokenParser(sourceCode, tokenStream));
        return sourceCode.currentChar();
    }
}
