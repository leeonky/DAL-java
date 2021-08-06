package com.github.leeonky.dal.token;

import com.github.leeonky.dal.parser.ParsingContext;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class BeginBracketTokenFactoryTest {
    private final TokenFactory tokenFactory = TokenFactory.createBeginBracketTokenFactory();

    @Test
    void return_empty_when_does_not_match_code() {
        final SourceCode sourceCode = new SourceCode("[");
        assertThat(tokenFactory.fetchToken(new ParsingContext(sourceCode, null)))
                .isNull();
    }

    @Test
    void return_token_when_matches_code() {
        final SourceCode sourceCode = new SourceCode("(");
        assertThat(tokenFactory.fetchToken(new ParsingContext(sourceCode, null)))
                .isEqualTo(Token.beginBracketToken());
    }

    @Test
    void should_take_char_when_matches_code() {
        SourceCode sourceCode = new SourceCode("(1");
        tokenFactory.fetchToken(new ParsingContext(sourceCode, null));

        assertThat(sourceCode.currentChar()).isEqualTo('1');
    }
}
