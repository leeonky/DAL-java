package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.SequenceNode;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.ExpressionConstructor;
import com.github.leeonky.interpreter.NodeParser;
import com.github.leeonky.interpreter.Procedure;
import com.github.leeonky.interpreter.SourceCode;

import java.util.LinkedList;
import java.util.Optional;
import java.util.function.Supplier;

import static java.util.Collections.singleton;

public class DALProcedure extends Procedure<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> {

    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));

    public DALProcedure(SourceCode sourceCode, DALRuntimeContext runtimeContext,
                        ExpressionConstructor<DALRuntimeContext, DALNode, DALExpression, DALOperator> expressionConstructor) {
        super(sourceCode, runtimeContext, expressionConstructor);
    }

    public Optional<DALNode> disableCommaAnd(Supplier<Optional<DALNode>> nodeFactory) {
        return commaAnd(false, nodeFactory);
    }

    public static NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> disableCommaAnd(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return procedure -> procedure.commaAnd(false, () -> nodeParser.parse(procedure));
    }

    public static NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> enableCommaAnd(
            NodeParser<DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure> nodeParser) {
        return procedure -> procedure.commaAnd(true, () -> nodeParser.parse(procedure));
    }

    public Optional<DALNode> enableCommaAnd(Supplier<Optional<DALNode>> nodeFactory) {
        return commaAnd(true, nodeFactory);
    }

    private Optional<DALNode> commaAnd(boolean b, Supplier<Optional<DALNode>> nodeFactory) {
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

    public Supplier<Optional<? extends SequenceNode>> sequenceOf(String sequenceChar, SequenceNode.Type type) {
        return () -> getSourceCode().repeatWords(sequenceChar, count -> new SequenceNode(count, type, sequenceChar));
    }

    public boolean notCodeBeginning() {
        return !isCodeBeginning();
    }

    public boolean isCodeBeginning() {
        return getSourceCode().isBeginning();
    }

    public boolean mayBeUnEqual() {
        return getSourceCode().startsWith("!=");
    }
}
