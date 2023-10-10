package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class TextFormatterTest {
    private final RuntimeContextBuilder.DALRuntimeContext context = new RuntimeContextBuilder().build(null);

    @Nested
    class NewLine {
        TextFormatter textFormatter = new TextFormatter() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return attribute.newLine("new-line");
            }

            @Override
            protected Object format(Object content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return attribute.newLine();
            }
        };

        @Test
        void reference_in_format() {

            assertThat(textFormatter.format(null, context)).isEqualTo("new-line");
        }

        @Test
        void keep_in_merged_formatter() {
            TextFormatter<Object, Object> another = new TextFormatter<Object, Object>() {
            };

            assertThat(textFormatter.merge(another).format(null, context)).isEqualTo("new-line");
        }

        @Test
        void replace_in_merged_formatter() {
            TextFormatter<Object, Object> overrideNewline = new TextFormatter<Object, Object>() {
                @Override
                protected TextAttribute attribute(TextAttribute attribute) {
                    return attribute.newLine("override-new-line");
                }
            };

            assertThat(textFormatter.merge(overrideNewline).format(null, context)).isEqualTo("override-new-line");
        }
    }

    @Nested
    class EndOfLine {
        TextFormatter<Object, Object> textFormatter = new TextFormatter<Object, Object>() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return attribute.endOfLine("end-of-line");
            }

            @Override
            protected Object format(Object content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return attribute.endOfLine();
            }
        };

        @Test
        void reference_property_in_format() {
            assertThat(textFormatter.format(null, context)).isEqualTo("end-of-line");
        }

        @Test
        void keep_in_merged_formatter() {
            TextFormatter<Object, Object> another = new TextFormatter<Object, Object>() {
            };

            assertThat(textFormatter.merge(another).format(null, context)).isEqualTo("end-of-line");
        }

        @Test
        void replace_in_merged_formatter() {
            TextFormatter<Object, Object> overrideNewline = new TextFormatter<Object, Object>() {
                @Override
                protected TextAttribute attribute(TextAttribute attribute) {
                    return attribute.endOfLine("override-enf-of-line");
                }
            };

            assertThat(textFormatter.merge(overrideNewline).format(null, context)).isEqualTo("override-enf-of-line");
        }
    }

    @Nested
    class Continuation {
        TextFormatter textFormatter = new TextFormatter() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return attribute.continuation("continuation");
            }

            @Override
            protected Object format(Object content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                return attribute.continuation();
            }
        };

        @Test
        void reference_property_in_format() {
            assertThat(textFormatter.format(null, context)).isEqualTo("continuation");
        }

        @Test
        void keep_in_merged_formatter() {
            TextFormatter<Object, Object> another = new TextFormatter<Object, Object>() {
            };

            assertThat(textFormatter.merge(another).format(null, context)).isEqualTo("continuation");
        }

        @Test
        void replace_in_merged_formatter() {
            TextFormatter<Object, Object> overrideNewline = new TextFormatter<Object, Object>() {
                @Override
                protected TextAttribute attribute(TextAttribute attribute) {
                    return attribute.continuation("override-continuation");
                }
            };

            assertThat(textFormatter.merge(overrideNewline).format(null, context)).isEqualTo("override-continuation");
        }
    }

    @Nested
    class formatPipeline {

        @Test
        void pipeline_format_method() {
            TextFormatter<Object, Object> textFormatter = new TextFormatter<Object, Object>() {
                @Override
                protected Object format(Object content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                    return content.toString().toLowerCase();
                }
            };

            TextFormatter<Object, Object> next = new TextFormatter<Object, Object>() {
                @Override
                protected Object format(Object content, TextAttribute attribute, RuntimeContextBuilder.DALRuntimeContext context) {
                    String s = content.toString();
                    return s.substring(0, 1).toUpperCase() + s.substring(1);
                }
            };

            assertThat(textFormatter.merge(next).format("HELLO", context)).isEqualTo("Hello");
        }

        @Test
        void merged_format_use_first_format_accept_type_and_last_format_return_type() {

            TextFormatter<String, Integer> first = new TextFormatter<String, Integer>() {
            };

            TextFormatter<Integer, List<String>> second = new TextFormatter<Integer, List<String>>() {
            };

            TextFormatter<String, List<String>> merge = first.merge(second);
            assertThat(merge.acceptType()).isEqualTo(String.class);

            assertThat(merge.returnType()).isEqualTo(List.class);
        }
    }
}
