package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.interpreter.TokenScanner;

import java.util.List;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.Notations.Keywords;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.SourceCode.tokenScanner;
import static java.util.Collections.emptySet;

public class Tokens {
    public static final TokenScanner<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            NUMBER = tokenScanner(DIGITAL::contains, emptySet(), false, Tokens::notNumber, Token::isNumber),
            INTEGER = tokenScanner(DIGITAL_OR_MINUS::contains, emptySet(), false, Tokens::notNumber, Token::isNumber),
            SYMBOL = tokenScanner(not(DELIMITER_OR_DOT::contains), Keywords.ALL_STRING, false, DELIMITER_OR_DOT, not(Token::isNumber)),
            DOT_SYMBOL = tokenScanner(not(DELIMITER_OR_DOT::contains), emptySet(), false, DELIMITER_OR_DOT, not(Token::isNumber)),
            RELAX_SYMBOL = tokenScanner(not(RELAX_DELIMITER::contains), Keywords.ALL_STRING, false, RELAX_DELIMITER, not(Token::isNumber)),
            RELAX_DOT_SYMBOL = tokenScanner(not(RELAX_DELIMITER::contains), emptySet(), false, RELAX_DELIMITER, not(Token::isNumber)),
            SCHEMA = tokenScanner(not(DELIMITER::contains), Keywords.ALL_STRING, false, DELIMITER, not(Token::isNumber));

    public static final TokenScanner.Mandatory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression,
            DALOperator, DALProcedure>
            EXPRESSION_RELAX_STRING = relaxString(EXPRESSION_RELAX_STRING_TAIL),
            OBJECT_SCOPE_RELAX_STRING = relaxString(OBJECT_SCOPE_RELAX_STRING_TAIL),
            LIST_SCOPE_RELAX_STRING = relaxString(LIST_SCOPE_RELAX_STRING_TAIL),
            TABLE_CELL_RELAX_STRING = relaxString(TABLE_CELL_RELAX_STRING_TAIL);

    private static TokenScanner.Mandatory<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator,
            DALProcedure> relaxString(List<String> expressionRelaxStringTail) {
        return tokenScanner(false, (code, position, size) -> expressionRelaxStringTail.stream()
                .anyMatch(tail -> code.startsWith(tail, position)));
    }

    private static boolean notNumber(String code, int position, int size) {
        if (size == 0)
            return false;
        return notSymbolAfterPower(code, position) || notNumberPoint(code, position);
    }

    private static boolean notNumberPoint(String code, int position) {
        return code.charAt(position) == '.' && (position == code.length() - 1
                || !DIGITAL.contains(code.charAt(position + 1)));
    }

    private static boolean notSymbolAfterPower(String code, int position) {
        char current = code.charAt(position);
        char lastChar = code.charAt(position - 1);
        return (DELIMITER.contains(current) && notSymbolAfterPower(lastChar, current))
                || (lastChar == '.' && !DIGITAL.contains(current));
    }

    private static boolean notSymbolAfterPower(Character lastChar, Character nextChar) {
        return (lastChar != 'e' && lastChar != 'E') || (nextChar != '-' && nextChar != '+');
    }
}
