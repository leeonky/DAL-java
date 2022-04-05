package com.github.leeonky.dal.compiler;

import com.github.leeonky.dal.ast.DALExpression;
import com.github.leeonky.dal.ast.DALNode;
import com.github.leeonky.dal.ast.DALOperator;
import com.github.leeonky.dal.runtime.RuntimeContextBuilder;
import com.github.leeonky.interpreter.Token;
import com.github.leeonky.interpreter.TokenScanner;

import static com.github.leeonky.dal.compiler.Constants.*;
import static com.github.leeonky.dal.compiler.Notations.Keywords;
import static com.github.leeonky.interpreter.FunctionUtil.not;
import static com.github.leeonky.interpreter.SourceCode.tokenScanner;
import static java.util.Collections.emptySet;

public class Tokens {
    public static final TokenScanner<RuntimeContextBuilder.DALRuntimeContext, DALNode, DALExpression, DALOperator, DALProcedure>
            NUMBER = tokenScanner(DIGITAL::contains, emptySet(), false, Tokens::notNumber, Token::isNumber),
            INTEGER = tokenScanner(DIGITAL_OR_MINUS::contains, emptySet(), false, DELIMITER, Token::isNumber),
            SYMBOL = tokenScanner(not(DELIMITER::contains), Keywords.ALL_STRING, false, DELIMITER_OR_DOT, not(Token::isNumber)),
            DOT_SYMBOL = tokenScanner(not(DELIMITER::contains), emptySet(), false, DELIMITER_OR_DOT, not(Token::isNumber)),
            SCHEMA = tokenScanner(not(DELIMITER::contains), Keywords.ALL_STRING, false, DELIMITER, not(Token::isNumber)),

    //    TODO do not limit start
    EXPRESSION_RELAX_STRING = tokenScanner(not(DELIMITER_OR_DOT::contains), Keywords.ALL_STRING, false,
            Tokens::relaxStringEnd, not(Token::isNumber));

    private static boolean relaxStringEnd(String code, Integer position) {
        return RELAX_STRING_TAIL.contains(code.charAt(position)) || code.startsWith("&&", position)
                || code.startsWith("||", position);
    }

    private static boolean notNumber(String code, int position) {
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
