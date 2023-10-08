package com.github.leeonky.dal.runtime;

public abstract class CustomizedTextFormatter<F, T> extends TextFormatter<F, T> {
    public static final TextFormatter<String, String> BASE_FORMATTER = new TextFormatter<String, String>() {
        @Override
        protected String format(String content, TextAttribute attribute) {
            String text = content.replace(attribute.endOfLine() + "\n", attribute.newLine())
                    .replace(attribute.continuation() + "\n", "")
                    .replace("\n", attribute.newLine());
            if (text.endsWith(attribute.endOfLine()))
                return text.substring(0, text.length() - attribute.endOfLine().length());
            return text;
        }
    };

    public static final CustomizedTextFormatter<String, String> DEFAULT_NEW_LINE = new CustomizedTextFormatter<String, String>() {
        @Override
        public String description() {
            return "use \\n as new line";
        }

        @Override
        protected TextAttribute attribute(TextAttribute attribute) {
            return attribute.newLine("\n");
        }
    };

    public static final CustomizedTextFormatter<String, String> DEFAULT_END_OF_LINE = new CustomizedTextFormatter<String, String>() {
        @Override
        public String description() {
            return "use < as end of line character";
        }

        @Override
        protected TextAttribute attribute(TextAttribute attribute) {
            return attribute.endOfLine("<");
        }
    };

    public static final CustomizedTextFormatter<String, String> DEFAULT_CONTINUE_CHAR = new CustomizedTextFormatter<String, String>() {
        @Override
        public String description() {
            return "use \\ as line continuation character";
        }

        @Override
        protected TextAttribute attribute(TextAttribute attribute) {
            return attribute.continuation("\\");
        }
    };

    public static final TextFormatter<String, String> DEFAULT = BASE_FORMATTER.merge(DEFAULT_NEW_LINE)
            .merge(DEFAULT_END_OF_LINE).merge(DEFAULT_CONTINUE_CHAR);

    public String description() {
        return "a customized formatter";
    }
}
