package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.node.DALExpression;
import com.github.leeonky.dal.ast.node.DALNode;
import com.github.leeonky.dal.ast.opt.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.NodeParser;
import com.github.leeonky.interpreter.NodeParser.Mandatory;
import com.github.leeonky.interpreter.Procedure;
import com.github.leeonky.interpreter.SourceCode;
import com.github.leeonky.interpreter.StringWithPosition;

import java.util.LinkedList;
import java.util.Optional;
import java.util.function.Supplier;

import static com.github.leeonky.dal.compiler.Notations.OPENING_GROUP;
import static com.github.leeonky.dal.compiler.Notations.Operators.NOT_EQUAL;
import static java.util.Collections.singleton;

public class DALProcedure extends Procedure<DALRuntimeContext, DALNode, DALExpression, DALOperator> {
    private final LinkedList<Boolean> enableAndComma = new LinkedList<>(singleton(true));
    private final DALRuntimeContext runtimeContext;

    private boolean enableSlashProperty = false, enableRelaxProperty = false, enableNumberProperty = false;

    public DALProcedure(SourceCode sourceCode, DALRuntimeContext runtimeContext) {
        super(sourceCode, runtimeContext);
        this.runtimeContext = runtimeContext;
    }

    public static NodeParser<DALNode, DALProcedure> disableCommaAnd(NodeParser<DALNode, DALProcedure> nodeParser) {
        return procedure -> procedure.commaAnd(false, () -> nodeParser.parse(procedure));
    }

    public static NodeParser<DALNode, DALProcedure> enableCommaAnd(NodeParser<DALNode, DALProcedure> nodeParser) {
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

    public boolean mayBeMetaProperty() {
        return getSourceCode().startsWith("::");
    }

    public static NodeParser<DALNode, DALProcedure> enableSlashProperty(NodeParser<DALNode, DALProcedure> nodeParser) {
        return procedure -> procedure.enableSlashProperty(() -> nodeParser.parse(procedure));
    }

    public static Mandatory<DALNode, DALProcedure> enableSlashProperty(Mandatory<DALNode, DALProcedure> mandatory) {
        return procedure -> procedure.enableSlashProperty(() -> mandatory.parse(procedure));
    }

    private <T> T enableSlashProperty(Supplier<T> supplier) {
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


    public static Mandatory<DALNode, DALProcedure> enableNumberProperty(Mandatory<DALNode, DALProcedure> mandatory) {
        return procedure -> procedure.enableNumberProperty(() -> mandatory.parse(procedure));
    }

    private <T> T enableNumberProperty(Supplier<T> supplier) {
        enableNumberProperty = true;
        try {
            return supplier.get();
        } finally {
            enableNumberProperty = false;
        }
    }

    public boolean isEnableNumberProperty() {
        return enableNumberProperty;
    }

    public static NodeParser<DALNode, DALProcedure> enableRelaxProperty(NodeParser<DALNode, DALProcedure> nodeParser) {
        return procedure -> procedure.enableRelaxProperty(() -> nodeParser.parse(procedure));
    }

    public static Mandatory<DALNode, DALProcedure> enableRelaxProperty(Mandatory<DALNode, DALProcedure> mandatory) {
        return procedure -> procedure.enableRelaxProperty(() -> mandatory.parse(procedure));
    }

    private <T> T enableRelaxProperty(Supplier<T> supplier) {
        enableRelaxProperty = true;
        try {
            return supplier.get();
        } finally {
            enableRelaxProperty = false;
        }
    }

    @Override
    public DALNode createExpression(DALNode left, DALOperator operator, DALNode right) {
        if (right.needPrefixBlankWarningCheck() && left.needPostBlankWarningCheck()) {
            int rightPosition = operator.getPosition() > 0 ? operator.getPosition() : right.getPositionBegin();
            int first = getSourceCode().chars().newlineBetween(left.getOperandPosition(), rightPosition);
            if (first != -1) {
                StringWithPosition stringWithPosition = new StringWithPosition(getSourceCode().chars().getCode());
                stringWithPosition.position(first).position(rightPosition);
                runtimeContext.warningOutput().append(stringWithPosition.result())
                        .append("\n\nWarning: may be ambiguous. Please add a comma or remove whitespace to clear this warning.");
            }
        }
        return DALExpression.expression(left, operator, right);
    }

}
