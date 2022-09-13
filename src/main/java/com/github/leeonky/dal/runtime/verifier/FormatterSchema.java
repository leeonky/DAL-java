package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.util.BeanClass;

import static com.github.leeonky.dal.runtime.verifier.SchemaVerifier.errorLog;
import static com.github.leeonky.util.BeanClass.cast;

class FormatterSchema extends JavaValueSchema {
    private final Formatter<Object, Object> formatter;

    @SuppressWarnings("unchecked")
    public FormatterSchema(String subPrefix, BeanClass<?> type, Object schemaProperty, Data value) {
        super(subPrefix, type, schemaProperty, value);
        formatter = cast(expect, Formatter.class).orElseGet(() -> createFormatter(type));
    }

    @Override
    public boolean verify(RuntimeContextBuilder.DALRuntimeContext runtimeContext) {
        return formatter.isValid(actual.getInstance())
                || errorLog("Expecting field `%s` to be in `%s`, but was [%s]", subPrefix,
                formatter.getFormatterName(), actual.getInstance());
    }

    @SuppressWarnings("unchecked")
    private Formatter<Object, Object> createFormatter(BeanClass<?> type) {
        return (Formatter<Object, Object>) type.getTypeArguments(0)
                .<Object>map(t -> type.newInstance((Object) t.getType())).orElseGet(type::newInstance);
    }
}
