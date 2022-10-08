package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;

import java.lang.reflect.Type;
import java.time.*;
import java.util.Date;
import java.util.UUID;

//TODO renames to inspectors
public class Dumpers implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerValueDumper(String.class, this::dumpString)
                .registerValueDumper(Number.class, Object::toString)
                .registerValueDumper(Boolean.class, Object::toString)
                .registerValueDumper(boolean.class, Object::toString)

                .registerObjectDumper(UUID.class, Object::toString)
                .registerObjectDumper(Instant.class, Object::toString)
                .registerObjectDumper(Date.class, date -> date.toInstant().toString())
                .registerObjectDumper(LocalTime.class, LocalTime::toString)
                .registerObjectDumper(LocalDate.class, LocalDate::toString)
                .registerObjectDumper(LocalDateTime.class, LocalDateTime::toString)
                .registerObjectDumper(OffsetDateTime.class, OffsetDateTime::toString)
                .registerObjectDumper(ZonedDateTime.class, ZonedDateTime::toString)
                .registerObjectDumper(YearMonth.class, YearMonth::toString)
                .registerObjectDumper(Class.class, Class::getName)
        ;

//        TODO missing Test for numbers
        dal.getRuntimeContextBuilder().registerValueInspector(Type.class, String.class, Number.class, Boolean.class,
                UUID.class, Instant.class, Date.class, LocalTime.class, LocalDate.class, LocalDateTime.class,
                OffsetDateTime.class, ZonedDateTime.class, YearMonth.class
        );
    }

    private String dumpString(Object o) {
        return "\"" + o.toString().replace("\\", "\\\\").replace("\t", "\\t").replace("\b", "\\b").replace("\n", "\\n")
                .replace("\r", "\\r").replace("\f", "\\f").replace("'", "\\'").replace("\"", "\\\"") + "\"";
    }
}
