package com.github.leeonky.dal;

import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.dal.runtime.TextAttribute;
import com.github.leeonky.dal.runtime.TextFormatter;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

public class TextFormatterTest {

    @Test
    void support_merge_format() {
        DAL dal = new DAL().extend();
        dal.getRuntimeContextBuilder().registerTextFormatter("Int", new TextFormatter<String, Integer>() {
            @Override
            protected Integer format(String content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return new Integer(content);
            }
        });

        dal.getRuntimeContextBuilder().registerTextFormatter("INC", new TextFormatter<Integer, Integer>() {
            @Override
            protected Integer format(Integer content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return content + 1;
            }
        });

        dal.evaluate("", "``` Int INC\n" +
                "1\n" +
                "```= 2");
    }

    @Test
    void default_merged_formatter_message() {
        DAL dal = new DAL();
        dal.getRuntimeContextBuilder().registerTextFormatter("Int", new TextFormatter<String, Integer>() {
            @Override
            protected Integer format(String content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return new Integer(content);
            }
        });

        dal.getRuntimeContextBuilder().registerTextFormatter("INC", new TextFormatter<Integer, Integer>() {
            @Override
            protected Integer format(Integer content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return content + 1;
            }
        });
        dal.getRuntimeContextBuilder().mergeTextFormatter("MG", "Int", "INC");

        assertThatThrownBy(() -> dal.evaluate("", "``` not-exist\n" +
                "```")).hasMessage("Invalid text formatter `not-exist`, all supported formatters are:\n" +
                "  Int:\n" +
                "    a customized formatter\n" +
                "        Accept: java.lang.String\n" +
                "        Return: java.lang.Integer\n" +
                "  INC:\n" +
                "    a customized formatter\n" +
                "        Accept: java.lang.Integer\n" +
                "        Return: java.lang.Integer\n" +
                "  MG:\n" +
                "    Merged from Int INC\n" +
                "        Accept: java.lang.String\n" +
                "        Return: java.lang.Integer");
    }

    @Test
    void invalid_type_when() {
        DAL dal = new DAL();
        dal.getRuntimeContextBuilder().registerTextFormatter("INC", new TextFormatter<Integer, Integer>() {
            @Override
            protected Integer format(Integer content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return content + 1;
            }
        });

        assertThat((int) dal.evaluate(null, "``` INC\n" +
                "1\n" +
                "```")).isEqualTo(2);
    }
}
