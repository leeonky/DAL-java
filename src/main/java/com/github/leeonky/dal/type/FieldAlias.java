package com.github.leeonky.dal.type;

import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

@Repeatable(FieldAliases.class)
@Target(TYPE)
@Retention(RUNTIME)
public @interface FieldAlias {
    String alias();

    String field();
}
