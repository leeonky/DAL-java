package com.github.leeonky.dal.parser;

import com.github.leeonky.dal.token.SourceCode;
import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.parser.SourceCodeGetter.DEFAULT;
import static org.assertj.core.api.Assertions.assertThat;

class SourceCodeGetterTest {

    @Test
    void get_char_one_char_from_source_code() {
        SourceCode sourceCode = new SourceCode("ab");
        assertThat(DEFAULT.getChar(sourceCode)).isEqualTo('a');
        assertThat(DEFAULT.getChar(sourceCode)).isEqualTo('b');

    }

    @Test
    void support_escape() {
        SourceCode sourceCode = new SourceCode("ab=cd");
        SourceCodeGetter taker = DEFAULT
                .escape("ab", 'x')
                .escape("cd", 'y');

        assertThat(taker.getChar(sourceCode)).isEqualTo('x');
        assertThat(taker.getChar(sourceCode)).isEqualTo('=');
        assertThat(taker.getChar(sourceCode)).isEqualTo('y');
    }
}
