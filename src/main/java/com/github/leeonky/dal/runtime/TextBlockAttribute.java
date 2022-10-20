package com.github.leeonky.dal.runtime;

import static com.github.leeonky.dal.util.TextUtil.lines;

public abstract class TextBlockAttribute {
    public static final TextBlockAttribute DEFAULT_NEW_LINE = new TextBlockAttribute() {
        @Override
        public String newLine() {
            return "\n";
        }

        @Override
        public String description() {
            return "use \\n as new line";
        }
    };
    public static final TextBlockAttribute DEFAULT_END_OF_LINE = new TextBlockAttribute() {
        @Override
        public String endOfLine() {
            return "<";
        }

        @Override
        public String description() {
            return "use < as end of line character";
        }
    };
    public static final TextBlockAttribute DEFAULT_CONTINUE_CHAR = new TextBlockAttribute() {

        @Override
        public String continuation() {
            return "\\";
        }

        @Override
        public String description() {
            return "use \\ as line continuation character";
        }
    };
    public static final TextBlockAttribute DEFAULT = DEFAULT_NEW_LINE.merge(DEFAULT_END_OF_LINE).merge(DEFAULT_CONTINUE_CHAR);

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
