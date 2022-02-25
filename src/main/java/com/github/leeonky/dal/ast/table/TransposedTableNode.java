package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

public class TransposedTableNode extends DALNode {
    private final PrefixHeadNode prefixHeadNode;
    private final List<TransposedRowNode> rows;

    @Deprecated
    public TransposedTableNode(List<DALNode> rows) {
        prefixHeadNode = null;
        this.rows = rows.stream().map(TransposedRowNode.class::cast).collect(toList());
    }

    public TransposedTableNode(DALNode prefix, List<DALNode> rows) {
        prefixHeadNode = (PrefixHeadNode) prefix;
        this.rows = rows.stream().map(TransposedRowNode.class::cast).collect(toList());
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
                    actualNode.evaluateData(context));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.columnPositionException(this);
        }
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
//        TODO refactor
        if (prefixHeadNode == null)
            return ">>" + rows.stream().map(TransposedRowNode::inspect).collect(Collectors.joining("\n"));
        return prefixHeadNode.inspect() + "\n" + rows.stream().map(TransposedRowNode::inspect).collect(Collectors.joining("\n"));
    }
}
