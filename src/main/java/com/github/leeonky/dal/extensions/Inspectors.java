package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.inspector.InspectorBk;
import com.github.leeonky.dal.runtime.inspector.InspectorContextBk;

import java.lang.reflect.Type;
import java.time.*;
import java.util.Date;
import java.util.UUID;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;
import static com.github.leeonky.dal.runtime.inspector.Dumper.STRING_DUMPER;
import static com.github.leeonky.dal.runtime.inspector.Dumper.VALUE_INSPECTOR;

@Order(BUILD_IN)
@SuppressWarnings("used")
public class Inspectors implements Extension {

    @Override
    public void extend(DAL dal) {
        registerValueTypes(dal, Type.class, Number.class, Boolean.class, UUID.class, Instant.class, Date.class,
                LocalTime.class, LocalDate.class, LocalDateTime.class, OffsetDateTime.class, ZonedDateTime.class,
                YearMonth.class);
//        TODO remove
        dal.getRuntimeContextBuilder().registerInspector(CharSequence.class, data -> new InspectorBk() {
            @Override
            public String inspect(Data data, InspectorContextBk context) {
                STRING_DUMPER.dumpDetail(data, context.dumpingContext());
                return "";
            }

            @Override
            public String dump(Data data, InspectorContextBk context) {
                STRING_DUMPER.dump(data, context.dumpingContext());
                return "";
            }
        });
    }

    private void registerValueTypes(DAL dal, Class<?>... types) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        for (Class<?> type : types)
            builder.registerInspector(type, data -> new InspectorBk() {
                @Override
                public String inspect(Data data, InspectorContextBk context) {
                    VALUE_INSPECTOR.dumpDetail(data, context.dumpingContext());
                    return "";
                }

                @Override
                public String dump(Data data, InspectorContextBk context) {
                    VALUE_INSPECTOR.dump(data, context.dumpingContext());
                    return "";
                }
            });
    }

}
