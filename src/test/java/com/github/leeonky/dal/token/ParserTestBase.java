package com.github.leeonky.dal.token;

import static org.assertj.core.api.Assertions.assertThat;

abstract class ParserTestBase {
    protected void assertString(String code, String expect) {
        assertThat(parse(code)).isEqualTo(expect);
    }

    protected String parse(String code) {
        return createParserWithCode(code).value();
    }

    protected TokenParser createParserWithCode(String code) {
        TokenParser parser = createParser();
        for (char c : code.toCharArray())
            parser.feed(c);
        return parser;
    }

    protected abstract TokenParser createParser();
}
