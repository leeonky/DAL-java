package com.github.leeonky.interpreter;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class TokenTest {

    @Test
    void get_integer() {
        assertThat(new Token(0).append("1y").getInteger().intValue()).isEqualTo(1);
        assertThat(new Token(0).append("1s").getInteger().intValue()).isEqualTo(1);
        assertThat(new Token(0).append("1l").getInteger().intValue()).isEqualTo(1);
        assertThat(new Token(0).append("1bi").getInteger().intValue()).isEqualTo(1);
    }

    @Test
    void raise_error_when_not_a_integer() {
        assertThrows(SyntaxException.class, () -> new Token(0).append("'100'").getInteger());
    }

    @Test
    void is_number() {
        assertThat(new Token(0).append("1").isNumber()).isTrue();
        assertThat(new Token(0).append("a").isNumber()).isFalse();
    }
}