package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.TextAttribute;

public class TextBlockAttributes implements Extension {
    public static final TextAttribute DEFAULT_NEW_LINE = new TextAttribute() {
        @Override
        public String newLine() {
            return "\n";
        }

        @Override
        public String description() {
            return "use \\n as new line";
        }
    };
    public static final TextAttribute DEFAULT_END_OF_LINE = new TextAttribute() {
        @Override
        public String tail() {
            return "<";
        }

        @Override
        public String description() {
            return "use < as end of line character";
        }
    };

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerTextBlockAttribute("LF", DEFAULT_NEW_LINE)
                .registerTextBlockAttribute("CR", new TextAttribute() {
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
                .registerTextBlockAttribute("⏎", new TextAttribute() {
                    @Override
                    public String tail() {
                        return "⏎";
                    }

                    @Override
                    public String description() {
                        return "use ⏎ as end of line character";
                    }
                });
    }
}
