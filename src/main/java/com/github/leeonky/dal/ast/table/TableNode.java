package com.github.leeonky.dal.ast.table;

import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.ast.ListScopeNode;
import com.github.leeonky.dal.ast.SortSequenceNode;
import com.github.leeonky.dal.runtime.ElementAssertionFailure;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder.DALRuntimeContext;
import com.github.leeonky.interpreter.Clause;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.github.leeonky.dal.ast.table.HeaderNode.bySequence;

public class TableNode extends DALNode {
    private final List<HeaderNode> headers;
    private final List<RowNode> rows;

    public TableNode(List<DALNode> headers, List<DALNode> rows) {
        this.headers = headers.stream().map(HeaderNode.class::cast).collect(Collectors.toList());
        this.rows = rows.stream().map(RowNode.class::cast).collect(Collectors.toList());
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
            return toListScope(operator).judgeAll(context,
                    actualNode.evaluateData(context).setListComparator(collectComparator(context)));
        } catch (ElementAssertionFailure elementAssertionFailure) {
            throw elementAssertionFailure.linePositionException();
        }
    }

    private ListScopeNode toListScope(DALOperator operator) {
        Stream<Clause<DALRuntimeContext, DALNode>> rowExpressionClauses =
                rows.stream().map(rowNode -> rowNode.toExpressionClause(operator));
        return new ListScopeNode(rowExpressionClauses.collect(Collectors.toList()), true);
    }

    @Override
    public String inspect() {
        return String.join("\n", new ArrayList<String>() {{
            add(printLine(headers));
            rows.stream().map(RowNode::inspect).forEach(this::add);
        }});
    }

    static String printLine(List<? extends DALNode> nodes) {
        return nodes.stream().map(DALNode::inspect).collect(Collectors.joining(" | ", "| ", " |"));
    }

    private Comparator<Object> collectComparator(DALRuntimeContext context) {
        return headers.stream().sorted(bySequence())
                .map(headerNode -> headerNode.getListComparator(context))
                .reduce(Comparator::thenComparing)
                .orElse(SortSequenceNode.NOP_COMPARATOR);
    }
}
