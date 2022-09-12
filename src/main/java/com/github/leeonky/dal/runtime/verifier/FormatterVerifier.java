package com.github.leeonky.dal.runtime.verifier;

import com.github.leeonky.dal.format.Formatter;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.util.BeanClass;

import java.util.Optional;

import static com.github.leeonky.util.BeanClass.cast;
import static java.util.Optional.of;

class FormatterVerifier implements FieldVerifier {
    private final Formatter<Object, Object> formatter;
    private final String subPrefix;

    @SuppressWarnings("unchecked")
    private FormatterVerifier(String subPrefix, BeanClass<?> type, Object schemaProperty) {
        formatter = cast(schemaProperty, Formatter.class).orElseGet(() -> createFormatter(type));
        this.subPrefix = subPrefix;
    }

    static Optional<FieldVerifier> createFormatterVerifier(String subPrefix, BeanClass<?> type, Object schemaProperty) {
        return Formatter.class.isAssignableFrom(type.getType()) ?
                of(new FormatterVerifier(subPrefix, type, schemaProperty)) : Optional.empty();
    }

    @SuppressWarnings("unchecked")
    private Formatter<Object, Object> createFormatter(BeanClass<?> type) {
        return (Formatter<Object, Object>) type.getTypeArguments(0)
                .<Object>map(t -> type.newInstance((Object) t.getType())).orElseGet(type::newInstance);
    }

    @Override
    public boolean verify(Data object) {
        return formatter.isValid(object.getInstance())
               || SchemaVerifier.errorLog("Expecting field `%s` to be in `%s`, but was [%s]", subPrefix,
                formatter.getFormatterName(), object.getInstance());
    }
}
