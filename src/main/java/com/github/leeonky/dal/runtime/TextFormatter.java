package com.github.leeonky.dal.runtime;

public class TextFormatter<F, T> {
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
        };
    }

    final public T format(F content) {
        return format(content, attribute(new TextAttribute("", "", "")));
    }
}
