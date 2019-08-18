package com.github.leeonky.dal.util;

import com.github.leeonky.dal.CompilingContextBuilder;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class WrappedObjectTest {

    @Test
    void check_null_for_customer_schema() {
        CompilingContextBuilder compilingContextBuilder = new CompilingContextBuilder().registerPropertyAccessor(AlwaysNull.class, new PropertyAccessor<AlwaysNull>() {
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

        assertTrue(compilingContextBuilder.wrap(new AlwaysNull()).isNull());
        assertTrue(compilingContextBuilder.wrap(null).isNull());
        assertFalse(compilingContextBuilder.wrap(new Object()).isNull());
    }

    static class AlwaysNull {
    }
}