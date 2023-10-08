package com.github.leeonky.dal.runtime;

import com.github.leeonky.util.BeanClass;

public abstract class TextFormatter<F, T> {

    @SuppressWarnings("unchecked")
    protected T format(F content, TextAttribute attribute) {
        return (T) content;
    }

    protected TextAttribute attribute(TextAttribute attribute) {
        return attribute;
    }

    final public <R> TextFormatter<F, R> merge(TextFormatter<T, R> another) {
        return new TextFormatter<F, R>() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return another.attribute(TextFormatter.this.attribute(attribute));
            }

            @Override
            protected R format(F content, TextAttribute attribute) {
                return another.format(TextFormatter.this.format(content, attribute), attribute);
            }

            @Override
            public Class<F> acceptType() {
                return TextFormatter.this.acceptType();
            }

            @Override
            public Class<R> returnType() {
                return another.returnType();
            }
        };
    }

    final public T format(F content) {
        return format(content, attribute(new TextAttribute("", "", "")));
    }

    @SuppressWarnings("unchecked")
    public Class<F> acceptType() {
        return (Class<F>) BeanClass.create(getClass()).getSuper(getClass().getSuperclass()).getTypeArguments(0)
                .orElseThrow(() -> new IllegalStateException("Cannot guess type via generic type argument, please override TextFormatter::acceptType"))
                .getType();
    }

    @SuppressWarnings("unchecked")
    public Class<T> returnType() {
        return (Class<T>) BeanClass.create(getClass()).getSuper(getClass().getSuperclass()).getTypeArguments(1)
                .orElseThrow(() -> new IllegalStateException("Cannot guess type via generic type argument, please override TextFormatter::returnType"))
                .getType();
    }
}
