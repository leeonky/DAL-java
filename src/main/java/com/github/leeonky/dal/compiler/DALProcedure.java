package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionFactory;
import com.github.leeonky.interpreter.NodeParser;
import com.github.leeonky.interpreter.Procedure;
import com.github.leeonky.interpreter.SourceCode;

import java.util.LinkedList;
import java.util.Optional;
import java.util.function.Supplier;

import static com.github.leeonky.dal.compiler.Notations.OPENING_GROUP;
import static com.github.leeonky.dal.compiler.Notations.Operators.NOT_EQUAL;
import static java.util.Collections.singleton;

public class DALProcedure extends Procedure<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {

    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));

    private boolean enableSlashProperty = false;

    private boolean enableRelaxProperty = false;

    public DALProcedure(SourceCode sourceCode, DALRuntimeContext runtimeContext,
                        ExpressionFactory<DALRuntimeContext, DALNode, DALExpression, DALOperator> expressionFactory) {
        super(sourceCode, runtimeContext, expressionFactory);
    }

    public static NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> disableCommaAnd(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return procedure -> procedure.commaAnd(false, () -> nodeParser.parse(procedure));
    }

    public static NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> enableCommaAnd(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return procedure -> procedure.commaAnd(true, () -> nodeParser.parse(procedure));
    }

    private <T> Optional<T> commaAnd(boolean b, Supplier<Optional<T>> nodeFactory) {
        enableAndComma.push(b);
        try {
            return nodeFactory.get();
        } finally {
            enableAndComma.pop();
        }
    }

    public boolean isEnableCommaAnd() {
        return enableAndComma.getFirst();
    }

    public boolean isCodeBeginning() {
        return getSourceCode().isBeginning();
    }

    public boolean mayBeUnEqual() {
        return getSourceCode().startsWith(NOT_EQUAL);
    }

    public boolean mayBeOpeningGroup() {
        return getSourceCode().startsWith(OPENING_GROUP);
    }

    public boolean mayBeElementEllipsis() {
        return getSourceCode().startsWith("..");
    }

    public boolean isEnableSlashProperty() {
        return enableSlashProperty;
    }

    public <T> T enableSlashProperty(Supplier<T> supplier) {
        enableSlashProperty = true;
        try {
            return supplier.get();
        } finally {
            enableSlashProperty = false;
        }
    }

    public boolean isEnableRelaxProperty() {
        return enableRelaxProperty;
    }

    public <T> T enableRelaxProperty(Supplier<T> supplier) {
        enableRelaxProperty = true;
        try {
            return supplier.get();
        } finally {
            enableRelaxProperty = false;
        }
    }
}
