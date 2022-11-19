package com.github.leeonky.dal.runtime.inspector;

import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class DumpingBufferTest {

    public static class Bean {
    }

    public static class BeanDumper implements Dumper {
        @Override
        public void dump(Data data, DumpingBuffer dumpingBuffer) {
            throw new RuntimeException("dump error");
        }
    }

    @Test
    void ignore_dump_error() {
        RuntimeContextBuilder builder = new RuntimeContextBuilder();
        builder.registerDumper(Bean.class, d -> new BeanDumper());
        RuntimeContextBuilder.DALRuntimeContext context = builder.build(null);

        assertThat(context.wrap(new Bean()).dumpValue()).isEqualTo("*dump throw* java.lang.RuntimeException: dump error");
        assertThat(context.wrap(new Bean()).dumpAll()).isEqualTo("*dump throw* java.lang.RuntimeException: dump error");
    }
}