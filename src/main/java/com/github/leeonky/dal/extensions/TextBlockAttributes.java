package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.TextBlockAttribute;

public class TextBlockAttributes implements Extension {
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

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerTextBlockAttribute("LF", DEFAULT_NEW_LINE)
                .registerTextBlockAttribute("CR", new TextBlockAttribute() {
                    @Override
                    public String newLine() {
                        return "\r";
                    }

                    @Override
                    public String description() {
                        return "use \\r as new line";
                    }
                })
                .registerTextBlockAttribute("<", DEFAULT_END_OF_LINE)
                .registerTextBlockAttribute("\\", DEFAULT_CONTINUE_CHAR)
                .registerTextBlockAttribute("⏎", new TextBlockAttribute() {
                    @Override
                    public String endOfLine() {
                        return "⏎";
                    }

                    @Override
                    public String description() {
                        return "use ⏎ as end of line character";
                    }
                });
    }
}
