package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TextFormatterTest {

    @Nested
    class NewLine {
        TextFormatter textFormatter = new TextFormatter() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return attribute.newLine("new-line");
            }

            @Override
            protected Object format(Object content, TextAttribute attribute) {
                return attribute.newLine();
            }
        };

        @Test
        void reference_in_format() {
            assertThat(textFormatter.format(null)).isEqualTo("new-line");
        }

        @Test
        void keep_in_merged_formatter() {
            TextFormatter another = new TextFormatter();

            assertThat(textFormatter.merge(another).format(null)).isEqualTo("new-line");
        }

        @Test
        void replace_in_merged_formatter() {
            TextFormatter overrideNewline = new TextFormatter() {
                @Override
                protected TextAttribute attribute(TextAttribute attribute) {
                    return attribute.newLine("override-new-line");
                }
            };

            assertThat(textFormatter.merge(overrideNewline).format(null)).isEqualTo("override-new-line");
        }
    }

    @Nested
    class EndOfLine {
        TextFormatter textFormatter = new TextFormatter() {
            @Override
            protected TextAttribute attribute(TextAttribute attribute) {
                return attribute.endOfLine("end-of-line");
            }

            @Override
            protected Object format(Object content, TextAttribute attribute) {
                return attribute.endOfLine();
            }
        };

        @Test
        void reference_property_in_format() {
            assertThat(textFormatter.format(null)).isEqualTo("end-of-line");
        }

        @Test
        void keep_in_merged_formatter() {
            TextFormatter another = new TextFormatter();

            assertThat(textFormatter.merge(another).format(null)).isEqualTo("end-of-line");
        }

        @Test
        void replace_in_merged_formatter() {
            TextFormatter overrideNewline = new TextFormatter() {
                @Override
                protected TextAttribute attribute(TextAttribute attribute) {
                    return attribute.endOfLine("override-enf-of-line");
                }
            };

            assertThat(textFormatter.merge(overrideNewline).format(null)).isEqualTo("override-enf-of-line");
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
            protected Object format(Object content, TextAttribute attribute) {
                return attribute.continuation();
            }
        };

        @Test
        void reference_property_in_format() {
            assertThat(textFormatter.format(null)).isEqualTo("continuation");
        }

        @Test
        void keep_in_merged_formatter() {
            TextFormatter another = new TextFormatter();

            assertThat(textFormatter.merge(another).format(null)).isEqualTo("continuation");
        }

        @Test
        void replace_in_merged_formatter() {
            TextFormatter overrideNewline = new TextFormatter() {
                @Override
                protected TextAttribute attribute(TextAttribute attribute) {
                    return attribute.continuation("override-continuation");
                }
            };

            assertThat(textFormatter.merge(overrideNewline).format(null)).isEqualTo("override-continuation");
        }
    }

    @Nested
    class formatPipeline {

        @Test
        void pipeline_format_method() {
            TextFormatter textFormatter = new TextFormatter() {
                @Override
                protected Object format(Object content, TextAttribute attribute) {
                    return content.toString().toLowerCase();
                }
            };

            TextFormatter next = new TextFormatter() {
                @Override
                protected Object format(Object content, TextAttribute attribute) {
                    String s = content.toString();
                    return s.substring(0, 1).toUpperCase() + s.substring(1);
                }
            };

            assertThat(textFormatter.merge(next).format("HELLO")).isEqualTo("Hello");
        }
    }
}
