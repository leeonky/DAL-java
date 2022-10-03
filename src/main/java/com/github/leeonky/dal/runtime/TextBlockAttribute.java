package com.github.leeonky.dal.runtime;

import static com.github.leeonky.dal.util.TextUtil.lines;

public abstract class TextBlockAttribute {
    public String newLine() {
        return null;
    }

    public String endOfLine() {
        return null;
    }

    public String continuation() {
        return null;
    }

    public TextBlockAttribute merge(TextBlockAttribute another) {
        return new TextBlockAttribute() {
            @Override
            public String newLine() {
                return chooseValue(TextBlockAttribute.this.newLine(), another.newLine());
            }

            @Override
            public String endOfLine() {
                return chooseValue(TextBlockAttribute.this.endOfLine(), another.endOfLine());
            }

            @Override
            public String continuation() {
                return chooseValue(TextBlockAttribute.this.continuation(), another.continuation());
            }

            @Override
            public String description() {
                return null;
            }

            private String chooseValue(String thisValue, String anotherValue) {
                return anotherValue == null ? thisValue : anotherValue;
            }
        };
    }

    public abstract String description();

    public Object format(String content) {
        String text = content.replace(endOfLine() + "\n", "\n").replace(continuation() + "\n", "");
        return text.isEmpty() ? text : String.join(newLine(), lines(text.substring(0, text.length() - 1)));
    }
}
