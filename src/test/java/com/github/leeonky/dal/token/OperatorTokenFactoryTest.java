package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class OperatorTokenFactoryTest {
    private final TokenFactory tokenFactory = new OperatorTokenFactory();

    @Test
    void return_empty_when_no_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(""), null))
                .isNull();
    }

    @Test
    void return_empty_when_first_char_is_not_char() {
        assertThat(tokenFactory.fetchToken(new SourceCode("not start with operator char"), null))
                .isNull();
    }


    @Test
    void should_return_operator_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode("+"), null))
                .isEqualTo(Token.operatorToken("+"));
    }

    @Test
    void should_return_empty_when_last_token_is_operator_matches() {
        assertThat(tokenFactory.fetchToken(new SourceCode("/"), Token.operatorToken("~")))
                .isNull();

        assertThat(tokenFactory.fetchToken(new SourceCode("+"), Token.operatorToken("~")))
                .isEqualTo(Token.operatorToken("+"));

        assertThat(tokenFactory.fetchToken(new SourceCode("/"), null))
                .isEqualTo(Token.operatorToken("/"));
    }

    @Test
    void should_rollback_last_end_char_in_source_code() {
        SourceCode sourceCode = new SourceCode("+1");
        tokenFactory.fetchToken(sourceCode, null);

        assertThat(sourceCode.getChar()).isEqualTo('1');
    }
}
