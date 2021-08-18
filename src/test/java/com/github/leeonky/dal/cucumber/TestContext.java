package com.github.leeonky.dal.cucumber;

import com.github.leeonky.dal.AssertionFailure;
import com.github.leeonky.dal.DataAssert;
import com.github.leeonky.dal.parser.TokenParser;
import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.Token;
import com.github.leeonky.dal.token.TokenFactory;

import static org.assertj.core.api.Assertions.assertThat;

public class TestContext {
    private final DataAssert dataAssert = new DataAssert();
    private Token token = null;
    private TokenParser tokenParser = null;
    private SourceCode sourceCode;

    public void givenDalSourceCode(String dalSourceCode) {
        sourceCode = new SourceCode(dalSourceCode);
        tokenParser = new TokenParser(sourceCode);
    }

    public void parseToken(TokenFactory tokenFactory) {
        token = tokenFactory.fetchToken(tokenParser);
    }

    public void assertToken(String assertion) {
        try {
            assertThat(dataAssert.assertData(token, assertion).isPassed()).isTrue();
        } catch (AssertionFailure failure) {
            System.err.println(failure.getMessage());
            System.err.println(failure.show(assertion));
            throw failure;
        }
    }

    public SourceCode getSourceCode() {
        return sourceCode;
    }
}
