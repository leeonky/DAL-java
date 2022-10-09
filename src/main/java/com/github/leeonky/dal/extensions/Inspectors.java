package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.inspector.StringInspector;

import java.lang.reflect.Type;
import java.time.*;
import java.util.Date;
import java.util.UUID;

public class Inspectors implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder().registerValueInspector(Type.class, Number.class, Boolean.class,
                UUID.class, Instant.class, Date.class, LocalTime.class, LocalDate.class, LocalDateTime.class,
                OffsetDateTime.class, ZonedDateTime.class, YearMonth.class);

        dal.getRuntimeContextBuilder().registerInspector(CharSequence.class, StringInspector::new);
    }
}
