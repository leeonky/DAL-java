package com.github.leeonky.dal;

import com.github.leeonky.dal.spec.Base;
import org.junit.jupiter.api.Test;

class ErrorHandler extends Base {

    @Test
    void should_raise_error_when_expected_a_value_node_but_not() {
        assertRuntimeException(1, "!", 1, "expect a value or expression");
        assertRuntimeException(1, " (=1)", 2, "expect a value or expression");
    }

    @Test
    void operand_of_operator_is_must_be_type() {
        assertRuntimeException(1, "is 1", 3, "operand of `is` must be schema type");
    }
}
