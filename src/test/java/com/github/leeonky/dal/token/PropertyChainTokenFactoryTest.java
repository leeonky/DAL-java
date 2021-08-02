package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class PropertyChainTokenFactoryTest {
    private final TokenFactory tokenFactory = new PropertyChainTokenFactory();

    @Test
    void return_empty_when_no_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(""), null))
                .isNull();
    }

    @Test
    void return_empty_when_not_match_single_quotation_string() {
        assertThat(tokenFactory.fetchToken(new SourceCode("not start with `.`"), null))
                .isNull();
    }

    @Test
    void should_return_property_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(".a"), null))
                .isEqualTo(Token.propertyToken("a"));
    }

    @Test
    void should_return_property_chain_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(".a.b"), null))
                .isEqualTo(Token.propertyToken("a", "b"));
    }

    @Test
    void seek_to_right_position_after_fetch_token() {
        SourceCode sourceCode = new SourceCode(".a=");

        tokenFactory.fetchToken(sourceCode, null);

        assertThat(sourceCode.getChar()).isEqualTo('=');
    }

    @Test
    void raise_error_when_token_is_empty() {
        assertThat(assertThrows(SyntaxException.class, () -> tokenFactory.fetchToken(new SourceCode("."), null)))
                .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 1);

        assertThat(assertThrows(SyntaxException.class, () -> tokenFactory.fetchToken(new SourceCode(". "), null)))
                .hasMessage("property chain not finished").hasFieldOrPropertyWithValue("position", 2);
    }
}
