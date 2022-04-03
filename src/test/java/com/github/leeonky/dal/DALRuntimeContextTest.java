package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.SchemaType;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

class DALRuntimeContextTest {

    @Test
    void should_raise_error_when_property_accessor_not_register() {
        RuntimeContextBuilder.DALRuntimeContext runtimeContext = new RuntimeContextBuilder().build(null);
        assertThrows(IllegalArgumentException.class, () -> runtimeContext.getPropertyValue(new Data(new Object(),
                new RuntimeContextBuilder().build(null), SchemaType.createRoot()), "anyc"));
    }
}