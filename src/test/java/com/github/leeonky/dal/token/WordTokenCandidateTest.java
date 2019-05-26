package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.keyWordToken;
import static com.github.leeonky.dal.token.Token.nullValueToken;
import static org.assertj.core.api.Assertions.assertThat;

class WordTokenCandidateTest {

    @Test
    void should_create_null_const_value_token() {
        assertToken("null", nullValueToken());
    }

    @Test
    void should_key_word_token() {
        assertToken("is", keyWordToken("is"));
        assertToken("which", keyWordToken("which"));
    }

    private void assertToken(String code, Token token) {
        SourceCode sourceCode = new SourceCode(code);
        assertThat(new WordTokenCandidate(sourceCode).getToken(sourceCode)).isEqualTo(token);
    }
}