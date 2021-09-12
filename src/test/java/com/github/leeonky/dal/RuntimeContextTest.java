package com.github.leeonky.dal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

class RuntimeContextTest {

    @Test
    void should_raise_error_when_property_accessor_not_register() {
        assertThrows(IllegalArgumentException.class, () ->
                new RuntimeContextBuilder().build(null).getPropertyValue(new Object(), "anyc"));
    }
}