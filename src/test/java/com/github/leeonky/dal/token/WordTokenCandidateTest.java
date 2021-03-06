package com.github.leeonky.dal.token;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.token.Token.*;
import static org.assertj.core.api.Assertions.assertThat;

class WordTokenCandidateTest {

    @Test
    void should_key_word_token() {
        assertToken("null", constValueToken(null));
        assertToken("is", keyWordToken("is"));
        assertToken("which", keyWordToken("which"));
        assertToken("true", constValueToken(true));
        assertToken("false", constValueToken(false));
        assertToken("and", operatorToken("&&"));
        assertToken("or", operatorToken("||"));
    }

    private void assertToken(String code, Token token) {
        SourceCode sourceCode = new SourceCode(code);
        assertThat(new WordTokenCandidate(sourceCode).getToken(sourceCode)).isEqualTo(token);
    }
}