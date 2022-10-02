package com.github.leeonky.dal.runtime;

public abstract class TextBlockAttribute {
    public String newLine() {
        return null;
    }

    public String tail() {
        return null;
    }

    public TextBlockAttribute merge(TextBlockAttribute another) {
        return new TextBlockAttribute() {
            @Override
            public String newLine() {
                return chooseValue(TextBlockAttribute.this.newLine(), another.newLine());
            }

            @Override
            public String tail() {
                return chooseValue(TextBlockAttribute.this.tail(), another.tail());
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
}
