package com.github.leeonky.dal;

import org.junit.jupiter.api.Test;

import static com.github.leeonky.dal.Assertions.expect;
import static org.assertj.core.api.Assertions.assertThat;

class AssertionsTest {

    @Test
    void given_dal_factory() {
        Assertions.setDalFactory(DAL::getInstance);

        expect(1).should("=1");
    }

    @Test
    void disable_dump_input() {
        try {
            Assertions.dumpInput(false);

            expect(1).should("=2");
        } catch (AssertionError error) {
            assertThat(error.getMessage()).doesNotContain("The root value was");
        } finally {
            Assertions.dumpInput(true);
        }
    }
}