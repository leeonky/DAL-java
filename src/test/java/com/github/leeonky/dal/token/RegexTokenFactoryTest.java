package com.github.leeonky.dal.token;

import com.github.leeonky.dal.SyntaxException;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RegexTokenFactoryTest {
    public static final Token OPT_MATCHES = Token.operatorToken("~");
    private final TokenFactory tokenFactory = new RegexTokenFactory();

    @Test
    void return_empty_when_no_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode(""), OPT_MATCHES))
                .isNull();
    }

    @Test
    void return_empty_when_not_match_single_quotation_string() {
        assertThat(tokenFactory.fetchToken(new SourceCode("not start with `/`"), OPT_MATCHES))
                .isNull();
    }

    @Test
    void return_empty_when_previous_token_is_not_opt_matches() {
        assertThat(tokenFactory.fetchToken(new SourceCode("/hello/"), null))
                .isNull();
    }

    @Test
    void should_return_regex_token_when_given_valid_code() {
        assertThat(tokenFactory.fetchToken(new SourceCode("/hello/"), OPT_MATCHES))
                .isEqualTo(Token.regexToken("hello"));
    }

    @Test
    void raise_error_when_string_not_complete() {
        assertThat(assertThrows(SyntaxException.class, () -> tokenFactory.fetchToken(new SourceCode("/hello"), OPT_MATCHES)))
                .hasMessage("regex should end with `/`").hasFieldOrPropertyWithValue("position", 6);
    }
}
