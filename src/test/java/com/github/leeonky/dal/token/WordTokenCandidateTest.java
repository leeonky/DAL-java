package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.constValueToken;
import static com.github.leeonky.dal.token.Token.keyWordToken;
import static org.assertj.core.api.Assertions.assertThat;

class WordTokenCandidateTest {

    @Test
    void should_create_null_const_value_token() {
        assertToken("null", constValueToken(null));
    }

    @Test
    void should_key_word_token() {
        assertToken("is", keyWordToken("is"));
        assertToken("which", keyWordToken("which"));
        assertToken("true", constValueToken(true));
        assertToken("false", constValueToken(false));
    }

    private void assertToken(String code, Token token) {
        SourceCode sourceCode = new SourceCode(code);
        assertThat(new WordTokenCandidate(sourceCode).getToken(sourceCode)).isEqualTo(token);
    }
}