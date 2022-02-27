package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.InterpreterException;
import com.github.leeonky.interpreter.SyntaxException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static com.github.leeonky.interpreter.InterpreterException.Position.Type.CHAR;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

public class TransposedTableNode extends DALNode {
    private final PrefixHeadNode prefixHeadNode;
    //TODO use a type
    private final List<TransposedRowNode> rows;

    public TransposedTableNode(DALNode prefix, List<DALNode> rows) {
        prefixHeadNode = (PrefixHeadNode) prefix;
        this.rows = rows.stream().map(TransposedRowNode.class::cast).collect(toList());

//        TODO refactor
        this.rows.forEach(prefixHeadNode::checkSize);
        Map<Integer, List<TransposedRowNode>> collect = this.rows.stream().collect(groupingBy(TransposedRowNode::cellCount));
        if (collect.size() > 1) {
            throw new SyntaxException("Different cell size", new ArrayList<>(collect.values()).get(1).get(0).getPositionBegin(),
                    InterpreterException.Position.Type.LINE);
        }

//        TODO refactor
        checkPrefix();
    }

    private void checkPrefix() {
        List<RowNode> rows = transposeRows();
        rows.stream().skip(1).filter(rowNode -> rowNode.samePrefix(rows.get(0))).findAny().ifPresent(row -> {
            throw new SyntaxException("Row index should be consistent", row.getPositionBegin(), CHAR)
                    .multiPosition(rows.get(0).getPositionBegin(), CHAR);
        });
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return judgeAsList(actualNode, operator, context);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return judgeAsList(actualNode, operator, context);
    }

    private boolean judgeAsList(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return new TableBody(transposeRows()).transformToListScope(operator).judgeAll(context,
                    actualNode.evaluateData(context).setListComparator(tableHead().collectComparator(context)));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
    }

    private TableHead tableHead() {
        return new TableHead(rows.stream().map(TransposedRowNode::getHeader).collect(toList()));
    }

    public List<RowNode> transposeRows() {
        return rows.stream().map(transposedRowNode -> transposedRowNode.transpose(prefixHeadNode))
                .reduce(this::merge).orElse(Collections.emptyList());
    }

    private List<RowNode> merge(List<RowNode> rows1, List<RowNode> rows2) {
        return new ArrayList<RowNode>() {{
            for (int i = 0; i < rows1.size(); i++)
                add(rows1.get(i).merge(rows2.get(i)));
        }};
    }

    @Override
    public String inspect() {
        return prefixHeadNode.inspect() + rows.stream().map(TransposedRowNode::inspect).collect(Collectors.joining("\n"));
    }
}
