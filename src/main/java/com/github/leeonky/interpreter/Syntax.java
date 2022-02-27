package com.github.leeonky.interpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

import static com.github.leeonky.interpreter.IfThenFactory.when;
import static com.github.leeonky.interpreter.Notation.notation;
import static java.lang.String.format;

public abstract class Syntax<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
        O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
        MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T, R> {
    protected final BiFunction<P, Syntax<C, N, E, O, P, OP, MA, ?, ?>, List<T>> parser;

    protected Syntax(BiFunction<P, Syntax<C, N, E, O, P, OP, MA, ?, ?>, List<T>> parser) {
        this.parser = parser;
    }

    protected abstract boolean isClose(P procedure);

    protected abstract void close(P procedure);

    protected abstract boolean isSplitter(P procedure);

    @SuppressWarnings("unchecked")
    protected R parse(Syntax<C, N, E, O, P, OP, MA, T, R> syntax, Function<List<T>, N> factory) {
        return (R) (NodeParser.Mandatory<C, N, E, O, P>) procedure -> factory.apply(parser.apply(procedure, syntax));
    }

    public static class DefaultSyntax<C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T, R> extends Syntax<C, N, E, O, P, OP, MA, T, R> {

        public DefaultSyntax(BiFunction<P, Syntax<C, N, E, O, P, OP, MA, ?, ?>, List<T>> parser) {
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
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T, R> extends Syntax<C, N, E, O, P, OP, MA, T, R> {

        private final Syntax<C, N, E, O, P, OP, MA, T, R> syntax;

        public CompositeSyntax(Syntax<C, N, E, O, P, OP, MA, T, R> syntax) {
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
        protected R parse(Syntax<C, N, E, O, P, OP, MA, T, R> syntax, Function<List<T>, N> factory) {
            return this.syntax.parse(syntax, factory);
        }
    }

    public Syntax<C, N, E, O, P, OP, MA, T, R> endWith(Notation notation) {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, R>(this) {
            @Override
            public void close(P procedure) {
                if (!procedure.getSourceCode().popWord(notation).isPresent())
                    throw procedure.getSourceCode().syntaxError(format("should end with `%s`", notation.getLabel()), 0);
            }

            @Override
            public boolean isClose(P procedure) {
                return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWith(notation);
            }
        };
    }

    public Syntax<C, N, E, O, P, OP, MA, T, R> endWith(String closing) {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, R>(endWith(notation(closing))) {
            @Override
            public boolean isClose(P procedure) {
                return !procedure.getSourceCode().hasCode() || procedure.getSourceCode().startsWith(closing);
            }
        };
    }

    public Syntax<C, N, E, O, P, OP, MA, T, R> splitBy(Notation notation) {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, R>(this) {
            @Override
            public boolean isSplitter(P procedure) {
                return procedure.getSourceCode().popWord(notation).isPresent();
            }
        };
    }

    public Syntax<C, N, E, O, P, OP, MA, T, R> endWithLine() {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, R>(this) {
            private boolean isClose = false;

            @Override
            public boolean isClose(P procedure) {
                //                TODO need test
                return isClose = procedure.getSourceCode().isEndOfLine();
            }

            @Override
            public void close(P procedure) {
                if (!isClose)
//                TODO need test
                    throw procedure.getSourceCode().syntaxError("unexpected token", 0);
            }
        };
    }

    public Syntax<C, N, E, O, P, OP, MA, T, R> optionalSplitBy(Notation splitter) {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, R>(this) {

            @Override
            public boolean isSplitter(P procedure) {
                procedure.getSourceCode().popWord(splitter);
                return true;
            }
        };
    }

    public Syntax<C, N, E, O, P, OP, MA, T, R> mandatoryTailSplitBy(Notation notation) {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, R>(this) {

            @Override
            public boolean isSplitter(P procedure) {
                if (procedure.getSourceCode().popWord(notation).isPresent())
                    return true;
                throw procedure.getSourceCode().syntaxError(format("should end with `%s`", notation.getLabel()), 0);
            }
        };
    }

    @SuppressWarnings("unchecked")
    public Syntax<C, N, E, O, P, OP, MA, T, NodeParser<C, N, E, O, P>> atLeast(int count) {
        return new CompositeSyntax<C, N, E, O, P, OP, MA, T, NodeParser<C, N, E, O, P>>(
                (Syntax<C, N, E, O, P, OP, MA, T, NodeParser<C, N, E, O, P>>) this) {
            @Override
            protected NodeParser<C, N, E, O, P> parse(Syntax<C, N, E, O, P, OP, MA, T, NodeParser<C, N, E, O, P>> syntax,
                                                      Function<List<T>, N> factory) {
                return procedure -> {
                    List<T> list = parser.apply(procedure, syntax);
                    return when(list.size() >= count).optional(() -> factory.apply(list));
                };
            }
        };
    }

    public R as(Function<List<T>, N> factory) {
        return parse(this, factory);
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> Syntax<C, N, E, O, P, OP, MA, T,
            NodeParser.Mandatory<C, N, E, O, P>> many(MA mandatory) {
        return new DefaultSyntax<>((procedure, syntax) -> procedure.actionUnderIndex(() -> new ArrayList<T>() {{
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
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> Syntax<C, N, E, O, P, OP, MA, T,
            NodeParser.Mandatory<C, N, E, O, P>> many(OP parser) {
        return new DefaultSyntax<>((procedure, syntax) -> procedure.actionUnderIndex(() -> new ArrayList<T>() {{
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
}
