package com.github.leeonky.dal.extensions;

import com.github.leeonky.dal.DAL;
import com.github.leeonky.dal.runtime.Extension;
import com.github.leeonky.dal.runtime.Order;
import com.github.leeonky.dal.runtime.TextBlockAttribute;

import static com.github.leeonky.dal.runtime.Order.BUILD_IN;

@Order(BUILD_IN)
public class TextBlockAttributes implements Extension {

    @Override
    public void extend(DAL dal) {
        dal.getRuntimeContextBuilder()
                .registerTextBlockAttribute("LF", TextBlockAttribute.DEFAULT_NEW_LINE)
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
                .registerTextBlockAttribute("<", TextBlockAttribute.DEFAULT_END_OF_LINE)
                .registerTextBlockAttribute("\\", TextBlockAttribute.DEFAULT_CONTINUE_CHAR)
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
