package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.lang.reflect.Type;
import java.time.*;
import java.util.Date;
import java.util.UUID;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;
import static com.github.leeonky.dal.runtime.inspector.InspectorBk.STRING_INSPECTOR_BK;
import static com.github.leeonky.dal.runtime.inspector.InspectorBk.VALUE_INSPECTOR_BK;

@Order(BUILD_IN)
public class Inspectors implements Extension {

    @Override
    public void extend(DAL dal) {
        registerValueTypes(dal, Type.class, Number.class, Boolean.class, UUID.class, Instant.class, Date.class,
                LocalTime.class, LocalDate.class, LocalDateTime.class, OffsetDateTime.class, ZonedDateTime.class,
                YearMonth.class);
        dal.getRuntimeContextBuilder().registerInspector(CharSequence.class, data -> STRING_INSPECTOR_BK);
    }

    private void registerValueTypes(DAL dal, Class<?>... types) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        for (Class<?> type : types)
            builder.registerInspector(type, data -> VALUE_INSPECTOR_BK);
    }
}
