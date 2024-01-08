package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.TextAttribute;
import com.github.leeonky.dal.runtime.TextFormatter;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class TextFormatters implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerTextFormatter("LF", TextFormatter.DEFAULT_NEW_LINE)
                .registerTextFormatter("CR", new TextFormatter<String, String>() {
                    @Override
                    public String description() {
                        return "use \\r as new line";
                    }

                    @Override
                    protected TextAttribute attribute(TextAttribute attribute) {
                        return attribute.newLine("\r");
                    }
                })
                .registerTextFormatter("CRLF", new TextFormatter<String, String>() {
                    @Override
                    public String description() {
                        return "use \\r\\n as new line";
                    }

                    @Override
                    protected TextAttribute attribute(TextAttribute attribute) {
                        return attribute.newLine("\r\n");
                    }
                })
                .registerTextFormatter("<", TextFormatter.DEFAULT_END_OF_LINE)
                .registerTextFormatter("\\", TextFormatter.DEFAULT_CONTINUE_CHAR)
                .registerTextFormatter("⏎", new TextFormatter<String, String>() {
                    @Override
                    public String description() {
                        return "use ⏎ as end of line character";
                    }

                    @Override
                    protected TextAttribute attribute(TextAttribute attribute) {
                        return attribute.endOfLine("⏎");
                    }
                });
    }
}
