package com.github.leeonky.dal;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.Accessors.get;
import static org.assertj.core.api.Assertions.assertThat;

class AccessorsTest {

    @Test
    void given_dal_factory() {
        Accessors.setDalFactory(DAL::getInstance);

        assertThat((int) get("length").from("hello")).isEqualTo(5);
    }

    @Test
    void use_given_exist_dal() {
        assertThat((int) get("length").by(DAL.getInstance()).from("hello")).isEqualTo(5);
    }

    @Test
    void dump_input() {
        try {
            get("not-exist").from("hello");
        } catch (RuntimeException error) {
            assertThat(error.getMessage()).contains("The root value was");
        }
    }

    @Test
    void disable_dump_input() {
        try {
            Accessors.dumpInput(false);
            get("not-exist").from("hello");
        } catch (RuntimeException error) {
            assertThat(error.getMessage()).doesNotContain("The root value was");
        } finally {
            Accessors.dumpInput(true);
        }
    }
}