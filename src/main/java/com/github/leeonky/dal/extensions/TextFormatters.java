package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.CustomizedTextFormatter;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.TextAttribute;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class TextFormatters implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerTextFormatter("LF", CustomizedTextFormatter.DEFAULT_NEW_LINE)
                .registerTextFormatter("CR", new CustomizedTextFormatter() {
                    @Override
                    public String description() {
                        return "use \\r as new line";
                    }

                    @Override
                    protected TextAttribute attribute(TextAttribute attribute) {
                        return attribute.newLine("\r");
                    }
                })
                .registerTextFormatter("<", CustomizedTextFormatter.DEFAULT_END_OF_LINE)
                .registerTextFormatter("\\", CustomizedTextFormatter.DEFAULT_CONTINUE_CHAR)
                .registerTextFormatter("⏎", new CustomizedTextFormatter() {
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
