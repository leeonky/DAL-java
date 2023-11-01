package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.inspector.Dumper;
import com.github.leeonky.dal.runtime.inspector.DumpingBuffer;
import com.github.leeonky.dal.type.InputCode;
import com.github.leeonky.dal.type.InputValue;

import java.lang.reflect.Type;
import java.time.*;
import java.util.Date;
import java.util.UUID;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;
import static com.github.leeonky.dal.runtime.inspector.Dumper.STRING_DUMPER;
import static com.github.leeonky.dal.runtime.inspector.Dumper.VALUE_INSPECTOR;

@Order(BUILD_IN)
@SuppressWarnings("used")
public class Dumpers implements Extension {
    private static final StackTraceDumper STACK_TRACE_DUMPER = new StackTraceDumper();
    private static final InputValueDumper INPUT_VALUE_DUMPER = new InputValueDumper();
    private static final InputCodeDumper INPUT_CODE_DUMPER = new InputCodeDumper();

    @Override
    public void extend(DAL dal) {
        registerValueTypes(dal, Type.class, Number.class, Boolean.class, UUID.class, Instant.class, Date.class,
                LocalTime.class, LocalDate.class, LocalDateTime.class, OffsetDateTime.class, ZonedDateTime.class,
                YearMonth.class);
        dal.getRuntimeContextBuilder()
                .registerDumper(CharSequence.class, data -> STRING_DUMPER)
                .registerDumper(StackTraceElement[].class, data -> STACK_TRACE_DUMPER)
                .registerDumper(InputValue.class, data -> INPUT_VALUE_DUMPER)
                .registerDumper(InputCode.class, data -> INPUT_CODE_DUMPER)
        ;
    }

    private void registerValueTypes(DAL dal, Class<?>... types) {
        for (Class<?> type : types)
            dal.getRuntimeContextBuilder().registerDumper(type, data -> VALUE_INSPECTOR);
    }

    private static class StackTraceDumper implements Dumper {

        @Override
        public void dump(Data data, DumpingBuffer dumpingBuffer) {
            DumpingBuffer sub = dumpingBuffer.indent();
            data.list().values().forEach(s -> sub.newLine().append("at " + s.toString()));
        }
    }

    private static class InputValueDumper implements Dumper {

        @Override
        public void dump(Data data, DumpingBuffer dumpingBuffer) {
            dumpingBuffer.dump(dumpingBuffer.getRuntimeContext().wrap(((InputValue<?>) data.instance()).get()));
        }
    }

    private static class InputCodeDumper implements Dumper {

        @Override
        public void dump(Data data, DumpingBuffer dumpingBuffer) {
            dumpingBuffer.append("InputCode return value was ");
            InputCode<?> inputCode = (InputCode<?>) data.instance();
            try {
                dumpingBuffer.dump(dumpingBuffer.getRuntimeContext().wrap(inputCode.get()));
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
        }
    }
}
