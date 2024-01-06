package com.github.leeonky.dal.ast.node.table;

import com.github.leeonky.dal.ast.node.*;
import com.github.leeonky.dal.ast.opt.Factory;
import com.github.leeonky.dal.runtime.Data;
import com.github.leeonky.interpreter.Clause;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.node.DALExpression.expression;
import static com.github.leeonky.dal.ast.node.InputNode.INPUT_NODE;
import static com.github.leeonky.dal.ast.node.SymbolNode.Type.BRACKET;
import static com.github.leeonky.dal.ast.node.table.SpecifyIndexRowType.indexToExpression;
import static com.github.leeonky.dal.compiler.Notations.EMPTY;
import static com.github.leeonky.util.function.When.when;
import static java.util.stream.Collectors.toList;

public abstract class RowType {

    public abstract RowType merge(RowType another);

    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        throw new IllegalArgumentException();
    }

    protected RowType mergeBy(DefaultIndexRowType defaultIndexRowType) {
        throw new IllegalArgumentException();
    }

    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        throw new IllegalArgumentException();
    }

    public abstract DALNode constructVerificationNode(Data actual, Stream<Clause<DALNode>> rowClauses,
                                                      Comparator<Data> comparator);

    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return input;
    }
}

class EmptyTableRowType extends RowType {

    @Override
    public RowType merge(RowType another) {
        return another;
    }

    @Override
    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        return this;
    }

    @Override
    protected RowType mergeBy(DefaultIndexRowType defaultIndexRowType) {
        return this;
    }

    @Override
    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        return this;
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALNode>> rowClauses,
                                             Comparator<Data> comparator) {
        return actual.isList() ? new ListScopeNode(rowClauses.collect(toList()), comparator, ListScopeNode.Style.TABLE)
                : new ObjectScopeNode(Collections.emptyList());
    }
}

class SpecifyIndexRowType extends RowType {
    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        return specifyIndexRowType;
    }

    @Override
    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        return specifyPropertyRowType;
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALNode>> rowClauses,
                                             Comparator<Data> comparator) {
        List<DALNode> rowNodes = rowClauses.map(rowClause -> rowClause.expression(null))
                .collect(toList());
        if (actual.isList())
            return new ListScopeNode(rowNodes, ListScopeNode.Type.FIRST_N_ITEMS, comparator, ListScopeNode.Style.TABLE);
        return new ObjectScopeNode(rowNodes);
    }

    @Override
    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.flatMap(SpecifyIndexRowType::indexToExpression).orElseThrow(IllegalStateException::new);
    }

    static Optional<DALNode> indexToExpression(DALNode node) {
        return when(node instanceof ConstValueNode).optional(() -> expression(INPUT_NODE, Factory.executable(EMPTY),
                new SymbolNode(((ConstValueNode) node).getValue(), BRACKET).setPositionBegin(node.getPositionBegin())));
    }
}

class DefaultIndexRowType extends RowType {
    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    protected RowType mergeBy(DefaultIndexRowType defaultIndexRowType) {
        return defaultIndexRowType;
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALNode>> rowClauses,
                                             Comparator<Data> comparator) {
        return new ListScopeNode(rowClauses.collect(toList()), comparator, ListScopeNode.Style.TABLE);
    }
}

class SpecifyPropertyRowType extends RowType {

    @Override
    public RowType merge(RowType another) {
        return another.mergeBy(this);
    }

    @Override
    public DALNode constructVerificationNode(Data actual, Stream<Clause<DALNode>> rowClauses,
                                             Comparator<Data> comparator) {
        return new ObjectScopeNode(rowClauses.map(rowNode -> rowNode.expression(null)).collect(toList()));
    }

    @Override
    protected RowType mergeBy(SpecifyPropertyRowType specifyPropertyRowType) {
        return specifyPropertyRowType;
    }

    @Override
    protected RowType mergeBy(SpecifyIndexRowType specifyIndexRowType) {
        return this;
    }

    @Override
    public DALNode constructAccessingRowNode(DALNode input, Optional<DALNode> indexOrKey) {
        return indexOrKey.map(node -> indexToExpression(node).orElse(node)).orElseThrow(IllegalStateException::new);
    }
}
