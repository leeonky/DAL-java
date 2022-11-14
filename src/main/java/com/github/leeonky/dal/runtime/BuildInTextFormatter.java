package com.github.leeonky.dal.runtime;

import static com.github.leeonky.dal.util.TextUtil.lines;

public abstract class BuildInTextFormatter extends TextFormatter {
    public static final TextFormatter BASE_FORMATTER = new TextFormatter() {
        @Override
        protected Object format(Object content, TextAttribute attribute) {
            String text = content.toString()
                    .replace(attribute.endOfLine() + "\n", "\n")
                    .replace(attribute.continuation() + "\n", "");
            return text.isEmpty() ? text : String.join(attribute.newLine(), lines(text.substring(0, text.length() - 1)));
        }
    };

    public static final BuildInTextFormatter DEFAULT_NEW_LINE = new BuildInTextFormatter() {
        @Override
        public String description() {
            return "use \\n as new line";
        }

        @Override
        protected TextAttribute attribute(TextAttribute attribute) {
            return attribute.newLine("\n");
        }
    };

    public static final BuildInTextFormatter DEFAULT_END_OF_LINE = new BuildInTextFormatter() {
        @Override
        public String description() {
            return "use < as end of line character";
        }

        @Override
        protected TextAttribute attribute(TextAttribute attribute) {
            return attribute.endOfLine("<");
        }
    };

    public static final BuildInTextFormatter DEFAULT_CONTINUE_CHAR = new BuildInTextFormatter() {
        @Override
        public String description() {
            return "use \\ as line continuation character";
        }

        @Override
        protected TextAttribute attribute(TextAttribute attribute) {
            return attribute.continuation("\\");
        }
    };

    public static final TextFormatter DEFAULT = BASE_FORMATTER.merge(DEFAULT_NEW_LINE)
            .merge(DEFAULT_END_OF_LINE).merge(DEFAULT_CONTINUE_CHAR);

    public abstract String description();
}
