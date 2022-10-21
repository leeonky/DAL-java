package com.github.leeonky.dal.runtime;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class TextBlockAttributeFactoryTest {

    @Nested
    class NewLine {
        TextFormatter textFormatter = new TextFormatter() {
            @Override
            protected Attribute attribute(Attribute attribute) {
                return attribute.newLine("new-line");
            }

            @Override
            protected Object format(Object content, Attribute attribute) {
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
                protected Attribute attribute(Attribute attribute) {
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
            protected Attribute attribute(Attribute attribute) {
                return attribute.endOfLine("end-of-line");
            }

            @Override
            protected Object format(Object content, Attribute attribute) {
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
                protected Attribute attribute(Attribute attribute) {
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
            protected Attribute attribute(Attribute attribute) {
                return attribute.continuation("continuation");
            }

            @Override
            protected Object format(Object content, Attribute attribute) {
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
                protected Attribute attribute(Attribute attribute) {
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
                protected Object format(Object content, Attribute attribute) {
                    return content.toString().toLowerCase();
                }
            };

            TextFormatter next = new TextFormatter() {
                @Override
                protected Object format(Object content, Attribute attribute) {
                    String s = content.toString();
                    return s.substring(0, 1).toUpperCase() + s.substring(1);
                }
            };


            assertThat(textFormatter.merge(next).format("HELLO")).isEqualTo("Hello");
        }
    }

    private static class TextFormatter {
        protected Object format(Object content, Attribute attribute) {
            return content;
        }

        protected Attribute attribute(Attribute attribute) {
            return attribute;
        }

        final public Object format(Object content) {
            return format(content, attribute(new Attribute("", "", "")));
        }

        final public TextFormatter merge(TextFormatter another) {
            return new TextFormatter() {
                @Override
                protected Attribute attribute(Attribute attribute) {
                    return another.attribute(TextFormatter.this.attribute(attribute));
                }

                @Override
                protected Object format(Object content, Attribute attribute) {
                    return another.format(TextFormatter.this.format(content, attribute), attribute);
                }
            };
        }
    }

    private static class Attribute {
        private final String newLine, endOfLine, continuation;

        private Attribute(String newLine, String endOfLine, String continuation) {
            this.newLine = newLine;
            this.endOfLine = endOfLine;
            this.continuation = continuation;
        }

        public String newLine() {
            return newLine;
        }

        public Attribute newLine(String newLine) {
            return new Attribute(newLine, endOfLine, continuation);
        }

        public String endOfLine() {
            return endOfLine;
        }

        public Attribute endOfLine(String endOfLine) {
            return new Attribute(newLine, endOfLine, continuation);
        }

        public Attribute continuation(String continuation) {
            return new Attribute(newLine, endOfLine, continuation);
        }

        public String continuation() {
            return continuation;
        }
    }
}
