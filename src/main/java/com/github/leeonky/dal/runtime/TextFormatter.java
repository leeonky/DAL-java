package com.github.leeonky.dal.runtime;

public class TextFormatter {
    protected Object format(Object content, TextAttribute attribute) {
        return content;
    }

    protected TextAttribute attribute(TextAttribute attribute) {
        return attribute;
    }

    final public TextFormatter merge(TextFormatter another) {
        return new TextFormatter() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return another.attribute(TextFormatter.this.attribute(attribute));
            }

            @Override
            protected Object format(Object content, TextAttribute attribute) {
                return another.format(TextFormatter.this.format(content, attribute), attribute);
            }
        };
    }

    final public Object format(Object content) {
        return format(content, attribute(new TextAttribute("", "", "")));
    }
}
