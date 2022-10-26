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
import static com.github.leeonky.dal.runtime.inspector.Inspector.STRING_INSPECTOR;
import static com.github.leeonky.dal.runtime.inspector.Inspector.VALUE_INSPECTOR;

@Order(BUILD_IN)
public class Inspectors implements Extension {

    @Override
    public void extend(DAL dal) {
        registerValueTypes(dal, Type.class, Number.class, Boolean.class, UUID.class, Instant.class, Date.class,
                LocalTime.class, LocalDate.class, LocalDateTime.class, OffsetDateTime.class, ZonedDateTime.class,
                YearMonth.class);
        dal.getRuntimeContextBuilder().registerInspector(CharSequence.class, STRING_INSPECTOR);
    }

    private void registerValueTypes(DAL dal, Class<?>... types) {
        RuntimeContextBuilder builder = dal.getRuntimeContextBuilder();
        for (Class<?> type : types)
            builder.registerInspector(type, VALUE_INSPECTOR);
    }
}
