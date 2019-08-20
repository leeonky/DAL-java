package com.github.leeonky.dal.spec;

import org.junit.jupiter.api.Test;

class VerifyNumber extends Base {

    @Test
    void should_check_number() {
        assertPass(null, "1 is PositiveInteger");
        assertFailed(null, "'1' is PositiveInteger");
        assertFailed(null, "1.0 is PositiveInteger");
        assertFailed(null, "0 is PositiveInteger");
        assertFailed(null, "(-1) is PositiveInteger");
    }
}
