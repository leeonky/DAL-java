package com.github.leeonky.dal.runtime;

public abstract class TextAttribute {
    public String newLine() {
        return null;
    }

    public String tail() {
        return null;
    }

    public TextAttribute merge(TextAttribute another) {
        return new TextAttribute() {
            @Override
            public String newLine() {
                return chooseValue(TextAttribute.this.newLine(), another.newLine());
            }

            @Override
            public String tail() {
                return chooseValue(TextAttribute.this.tail(), another.tail());
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
