package com.github.leeonky.dal;

import com.github.leeonky.dal.compiler.SyntaxException;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

class DALTest {

    @Test
    void raise_error_when_give_multi_expression() {
        assertThat(assertThrows(SyntaxException.class, () -> new DAL().evaluate(null, "1 2")))
                .hasMessage("more than one expression")
                .hasFieldOrPropertyWithValue("position", 2);
    }

    DAL dal = new DAL();

    @Test
    void number_types() {
        assertThat((Object) dal.evaluate(null, "1y")).isEqualTo((byte) 1);
        assertThat((Object) dal.evaluate(null, "0x10y")).isEqualTo((byte) 16);
        assertThat((Object) dal.evaluate(null, "1Y")).isEqualTo((byte) 1);
        assertThat((Object) dal.evaluate(null, "0x10Y")).isEqualTo((byte) 16);

        assertThat((Object) dal.evaluate(null, "1s")).isEqualTo((short) 1);
        assertThat((Object) dal.evaluate(null, "0x10s")).isEqualTo((short) 16);
        assertThat((Object) dal.evaluate(null, "1S")).isEqualTo((short) 1);
        assertThat((Object) dal.evaluate(null, "0x10S")).isEqualTo((short) 16);

        assertThat((Object) dal.evaluate(null, "1l")).isEqualTo(1L);
        assertThat((Object) dal.evaluate(null, "0x10l")).isEqualTo(16L);
        assertThat((Object) dal.evaluate(null, "1L")).isEqualTo(1L);
        assertThat((Object) dal.evaluate(null, "0x10L")).isEqualTo(16L);

        assertThat((Object) dal.evaluate(null, "1d")).isEqualTo(1d);
        assertThat((Object) dal.evaluate(null, "1D")).isEqualTo(1d);

        assertThat((Object) dal.evaluate(null, "1f")).isEqualTo(1f);
        assertThat((Object) dal.evaluate(null, "1F")).isEqualTo(1f);

        assertThat((Object) dal.evaluate(null, "1bi")).isEqualTo(BigInteger.valueOf(1));
        assertThat((Object) dal.evaluate(null, "0x10bi")).isEqualTo(BigInteger.valueOf(16L));
        assertThat((Object) dal.evaluate(null, "1BI")).isEqualTo(BigInteger.valueOf(1L));
        assertThat((Object) dal.evaluate(null, "0x10BI")).isEqualTo(BigInteger.valueOf(16L));

        assertThat((Object) dal.evaluate(null, "1bd")).isEqualTo(BigDecimal.valueOf(1));
        assertThat((Object) dal.evaluate(null, "1BD")).isEqualTo(BigDecimal.valueOf(1));
    }
}