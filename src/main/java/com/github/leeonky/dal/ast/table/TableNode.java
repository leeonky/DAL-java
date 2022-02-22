package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;

import java.util.List;
import java.util.stream.Collectors;

public class TableNode extends DALNode {
    private final TableHead tableHead;
    private final TableBody tableBody;

    public TableNode(TableHead tableHead, TableBody tableBody) {
        this.tableHead = tableHead;
        this.tableBody = tableBody;
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Matcher operator, DALRuntimeContext context) {
        return judgeAsList(actualNode, operator, context);
    }

    @Override
    public boolean judge(DALNode actualNode, DALOperator.Equal operator, DALRuntimeContext context) {
        return judgeAsList(actualNode, operator, context);
    }

    private boolean judgeAsList(DALNode actualNode, DALOperator operator, DALRuntimeContext context) {
        try {
            return tableBody.transformToListScope(operator).judgeAll(context, actualNode.evaluateData(context)
                    .setListComparator(tableHead.collectComparator(context)));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.linePositionException();
        }
    }

    @Override
    public String inspect() {
        return tableHead.inspect() + tableBody.inspect();
    }

    public static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }
}
