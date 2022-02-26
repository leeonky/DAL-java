package com.github.leeonky.interpreter;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.compiler.DALProcedure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;
import java.util.function.Function;

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

    @SuppressWarnings("unchecked")
    public R as(Function<List<T>, N> factory) {
        return (R) (NodeParser.Mandatory<C, N, E, O, P>) procedure -> factory.apply(parser.apply(procedure, this));
    }

    public static <C extends RuntimeContext<C>, N extends Node<C, N>, E extends Expression<C, N, E, O>,
            O extends Operator<C, N, O>, P extends Procedure<C, N, E, O, P>, OP extends Parser<C, N, E, O, P, OP, MA, T>,
            MA extends Parser.Mandatory<C, N, E, O, P, OP, MA, T>, T> Syntax<C, N, E, O, P, OP, MA, T,
            NodeParser.Mandatory<C, N, E, O, P>> many(MA mandatory) {
        return new DefaultSyntax<>((procedure, syntax) -> procedure.actionUnderIndex(() -> new ArrayList<T>() {{
            while (!syntax.isClose(procedure)) {
                add(mandatory.parse(procedure));
                procedure.incrementIndex();
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
            }
            syntax.close(procedure);
        }}));
    }

    public static void xxx() {
        NodeParser.Mandatory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> X = null;
        many(X).endWith("\"").as(null);
//        new Syntax<>(X) {
//            @Override
//            protected List parse(Procedure procedure) {
//                return procedure.actionUnderIndex(() -> new ArrayList<T>() {{
//                    while (isClose(procedure)) {
//                        add(X.parse(procedure));
//                        procedure.incrementIndex();
//                    }
//                    close(procedure);
//                }});
//            }
//        }.endWith("");
    }
}
