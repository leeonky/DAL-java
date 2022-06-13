package com.github.leeonky.interpreter;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.Notation.notation;
import static java.lang.String.format;

public abstract class Syntax<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> {
    protected final BiFunction<P, Syntax<C, N, E, O, P, PA, MA, ?, ?, A>, A> parser;

    protected Syntax(BiFunction<P, Syntax<C, N, E, O, P, PA, MA, ?, ?, A>, A> parser) {
        this.parser = parser;
    }

    protected abstract boolean isClose(P procedure);

    protected abstract void close(P procedure);

    protected abstract boolean isSplitter(P procedure);

    @SuppressWarnings("unchecked")
    protected R parse(Syntax<C, N, E, O, P, PA, MA, T, R, A> syntax, Function<A, N> factory) {
        return (R) (NodeParser.Mandatory<C, N, E, O, P>) procedure -> factory.apply(parser.apply(procedure, syntax));
    }

    public static class DefaultSyntax<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> extends Syntax<C, N, E, O, P, PA, MA, T, R, A> {

        public DefaultSyntax(BiFunction<P, Syntax<C, N, E, O, P, PA, MA, ?, ?, A>, A> parser) {
            super(parser);
        }

        @Override
        protected boolean isClose(P procedure) {
            return false;
        }

        @Override
        protected void close(P procedure) {
        }

        @Override
        protected boolean isSplitter(P procedure) {
            return true;
        }
    }

    public static class CompositeSyntax<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> extends Syntax<C, N, E, O, P, PA, MA, T, R, A> {

        private final Syntax<C, N, E, O, P, PA, MA, T, R, A> syntax;

        public CompositeSyntax(Syntax<C, N, E, O, P, PA, MA, T, R, A> syntax) {
            super(syntax.parser);
            this.syntax = syntax;
        }

        @Override
        protected boolean isClose(P procedure) {
            return syntax.isClose(procedure);
        }

        @Override
        protected void close(P procedure) {
            syntax.close(procedure);
        }

        @Override
        protected boolean isSplitter(P procedure) {
            return syntax.isSplitter(procedure);
        }

        @Override
        protected R parse(Syntax<C, N, E, O, P, PA, MA, T, R, A> syntax, Function<A, N> factory) {
            return this.syntax.parse(syntax, factory);
        }
    }

    public <NR, NA> Syntax<C, N, E, O, P, PA, MA, T, NR, NA> and(Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
            Syntax<C, N, E, O, P, PA, MA, T, NR, NA>> rule) {
        return rule.apply(this);
    }

    public R as(Function<A, N> factory) {
        return parse(this, factory);
    }

    @SuppressWarnings("unchecked")
    public R as() {
        return parse(this, a -> (N) a);
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> Syntax<C, N, E, O, P, PA, MA, T,
            NodeParser<C, N, E, O, P>, T> single(PA parser) {
        return new DefaultSyntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, T>((procedure, syntax) -> {
            Optional<T> optional = parser.parse(procedure);
            if (optional.isPresent()) {
                syntax.isClose(procedure);
                syntax.close(procedure);
            }
            return optional.orElse(null);
        }) {
            @Override
            protected NodeParser<C, N, E, O, P> parse(Syntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>,
                    T> syntax, Function<T, N> factory) {
                return (P procedure) -> Optional.ofNullable(parser.apply(procedure, syntax)).map(factory);
            }
        };
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> Syntax<C, N, E, O, P, PA, MA, T,
            NodeParser.Mandatory<C, N, E, O, P>, T> single(MA parser) {
        return new DefaultSyntax<C, N, E, O, P, PA, MA, T, NodeParser.Mandatory<C, N, E, O, P>, T>((procedure, syntax) -> {
            T t = parser.parse(procedure);
            syntax.isClose(procedure);
            syntax.close(procedure);
            return t;
        }) {
            @Override
            protected NodeParser.Mandatory<C, N, E, O, P> parse(Syntax<C, N, E, O, P, PA, MA, T,
                    NodeParser.Mandatory<C, N, E, O, P>, T> syntax, Function<T, N> factory) {
                return (P procedure) -> factory.apply(parser.apply(procedure, syntax));
            }
        };
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> Syntax<C, N, E, O, P, PA, MA, T,
            NodeParser.Mandatory<C, N, E, O, P>, List<T>> many(MA mandatory) {
        return new DefaultSyntax<>((procedure, syntax) -> procedure.withIndex(() -> new ArrayList<T>() {{
            while (!syntax.isClose(procedure)) {
                add(mandatory.parse(procedure));
                procedure.incrementIndex();
                if (!syntax.isSplitter(procedure))
                    break;
            }
            syntax.close(procedure);
        }}));
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T> Syntax<C, N, E, O, P, PA, MA, T,
            NodeParser.Mandatory<C, N, E, O, P>, List<T>> many(PA parser) {
        return new DefaultSyntax<>((procedure, syntax) -> procedure.withIndex(() -> new ArrayList<T>() {{
            while (!syntax.isClose(procedure)) {
                Optional<T> optional = parser.parse(procedure);
                if (!optional.isPresent())
                    break;
                add(optional.get());
                procedure.incrementIndex();
                if (!syntax.isSplitter(procedure))
                    break;
            }
            syntax.close(procedure);
        }}));
    }

    public static class Rules {
        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> endWith(Notation notation) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax) {
                @Override
                public void close(P procedure) {
                    if (!procedure.getSourceCode().popWord(notation).isPresent())
                        throw procedure.getSourceCode().syntaxError(format("Should end with `%s`", notation.getLabel()), 0);
                }

                @Override
                public boolean isClose(P procedure) {
                    return procedure.getSourceCode().startsWith(notation) || !procedure.getSourceCode().hasCode();
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> endWith(String closing) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax.and(Rules.endWith(notation(closing)))) {
                @Override
                public boolean isClose(P procedure) {
                    return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWith(closing);
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> endWithLine() {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax) {
                private boolean isClose = false;

                @Override
                public boolean isClose(P procedure) {
                    isClose = procedure.getSourceCode().isEndOfLine();
                    if (isClose && procedure.getSourceCode().hasCode())
                        procedure.getSourceCode().popChar(Collections.emptyMap());
                    return isClose;
                }

                @Override
                public void close(P procedure) {
                    if (!isClose)
                        throw procedure.getSourceCode().syntaxError("unexpected token", 0);
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> splitBy(Notation notation) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax) {
                @Override
                public boolean isSplitter(P procedure) {
                    return procedure.getSourceCode().popWord(notation).isPresent();
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> endWithOptionalLine() {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax.and(Rules.endWithLine())) {
                @Override
                public void close(P procedure) {
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> optionalSplitBy(Notation splitter) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax) {
                @Override
                public boolean isSplitter(P procedure) {
                    procedure.getSourceCode().popWord(splitter);
                    return true;
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> mandatorySplitBy(Notation splitter) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax) {
                @Override
                public boolean isSplitter(P procedure) {
                    if (procedure.getSourceCode().popWord(splitter).isPresent())
                        return true;
                    throw procedure.getSourceCode().syntaxError(format("Should end with `%s`", splitter.getLabel()), 0);
                }
            };
        }

        @SuppressWarnings("unchecked")
        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, List<T>>> atLeast(int size) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, List<T>>(
                    (Syntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, List<T>>) syntax) {
                @Override
                protected NodeParser<C, N, E, O, P> parse(Syntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, List<T>> syntax,
                                                          Function<List<T>, N> factory) {
                    return procedure -> procedure.getSourceCode().tryFetch(() -> {
                        List<T> list = parser.apply(procedure, syntax);
                        return when(list.size() >= size).optional(() -> factory.apply(list));
                    });
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, A> Function<Syntax<C, N, E, O, P, PA, MA, T,
                NodeParser<C, N, E, O, P>, A>, Syntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, A>> enabledBefore(Notation notation) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, A>(syntax) {

                @Override
                protected NodeParser<C, N, E, O, P> parse(Syntax<C, N, E, O, P, PA, MA, T, NodeParser<C, N, E, O, P>, A> syntax,
                                                          Function<A, N> factory) {
                    NodeParser<C, N, E, O, P> nodeParser = super.parse(syntax, factory);
                    return procedure -> procedure.getSourceCode().tryFetch(() -> nodeParser.parse(procedure).map(node ->
                            procedure.getSourceCode().startsWith(notation) ? node : null));
                }
            };
        }

        public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
                O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, PA extends Parser<C, N, E, O, P, PA, MA, T>,
                MA extends Parser.Mandatory<C, N, E, O, P, PA, MA, T>, T, R, A> Function<Syntax<C, N, E, O, P, PA, MA, T, R, A>,
                Syntax<C, N, E, O, P, PA, MA, T, R, A>> endOfRow(Notation splitter) {
            return syntax -> new CompositeSyntax<C, N, E, O, P, PA, MA, T, R, A>(syntax) {
                private boolean isClose = false;

                @Override
                public boolean isClose(P procedure) {
                    isClose = procedure.getSourceCode().isEndOfLine() || !procedure.getSourceCode().hasCode();
                    if (isClose) {
                        if (procedure.getSourceCode().hasCode())
                            procedure.getSourceCode().popChar(Collections.emptyMap());
                    } else {
                        String code = procedure.getSourceCode().codeBefore(splitter);
                        isClose = code.contains("\r") || code.contains("\n");
                    }
                    return isClose;
                }

                @Override
                public void close(P procedure) {
                    if (!isClose)
                        throw procedure.getSourceCode().syntaxError("unexpected token", 0);
                }
            };
        }
    }
}
