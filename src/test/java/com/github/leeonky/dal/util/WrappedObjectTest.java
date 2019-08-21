package com.github.leeonky.dal.util;

import com.github.leeonky.dal.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WrappedObjectTest {

    @Test
    void check_null_for_customer_schema() {
        RuntimeContextBuilder runtimeContextBuilder = new RuntimeContextBuilder().registerPropertyAccessor(AlwaysNull.class, new PropertyAccessor<AlwaysNull>() {
            @Override
            public Object getValue(AlwaysNull instance, String name) {
                return null;
            }

            @Override
            public Set<String> getPropertyNames(AlwaysNull instance) {
                return null;
            }

            @Override
            public boolean isNull(AlwaysNull instance) {
                return true;
            }
        });

        assertTrue(runtimeContextBuilder.build(null).wrap(new AlwaysNull()).isNull());
        assertTrue(runtimeContextBuilder.build(null).wrap(null).isNull());
        assertFalse(runtimeContextBuilder.build(null).wrap(new Object()).isNull());
    }

    private static class AlwaysNull {
    }
}