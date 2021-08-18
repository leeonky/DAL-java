package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import com.github.leeonky.dal.token.TokenFactory;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.TokenContentInToken.byFactory;
import static com.github.leeonky.dal.parser.TokenParser.*;
import static com.github.leeonky.dal.token.TokenFactory.TOKEN_TREE;
import static com.github.leeonky.dal.token.TokenFactory.createIdentifierTokenFactory;
import static org.assertj.core.api.Assertions.assertThat;

class TokenParserTest {

    @Test
    void should_clean_parsed_code_when_create_sub_parser() {
        TokenParser tokenParser = new TokenParser(new SourceCode("100 |hello"));

        NewTokenFactory.startWith(included(ANY_CHARACTERS))
                .take(byFactory(TokenFactory.createNumberTokenFactory()))
                .endWith(excluded(CHARACTER('|'))).createAs(TOKEN_TREE).fetchToken(tokenParser);

        assertThat(createIdentifierTokenFactory().fetchToken(tokenParser).getValue()).isEqualTo("hello");
    }
}