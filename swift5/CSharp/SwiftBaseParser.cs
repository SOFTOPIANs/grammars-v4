/*import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.misc.Interval;

import java.util.BitSet;
*/

using System;
using System.Collections;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using PT.PM.SwiftParseTreeUst;

public abstract class SwiftBaseParser : Parser {

    public static int TERMINATOR2, WS2, COMMENT2;

    protected SwiftBaseParser(ITokenStream input) : base(input)
    {
    }
    /* TODO
    There is one caveat to the rules above. If the ! or ? predefined operator
     has no whitespace on the left, it is treated as a postfix operator,
     regardless of whether it has whitespace on the right. To use the ? as
     the optional-chaining operator, it must not have whitespace on the left.
      To use it in the ternary conditional (? :) operator, it must have
      whitespace around both sides.
    */

    /*
    operator-head : /  =  -  +  !  *  %  <  >  &  |  ^  ~  ?
      | [\u00A1-\u00A7]
      | [\u00A9\u00AB]
      | [\u00AC\u00AE]
      | [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
      | [\u2016-\u2017\u2020-\u2027]
      | [\u2030-\u203E]
      | [\u2041-\u2053]
      | [\u2055-\u205E]
      | [\u2190-\u23FF]
      | [\u2500-\u2775]
      | [\u2794-\u2BFF]
      | [\u2E00-\u2E7F]
      | [\u3001-\u3003]
      | [\u3008-\u3030]
      ;
     */
    public static BitArray operatorHead = new BitArray(0x10000)
    {

    };
    public static BitArray operatorCharacter;

    public static BitArray leftWS = new BitArray(255);
    public static BitArray rightWS = new BitArray(255);

    static SwiftBaseParser() {
        // operator-head → /  =­  -­  +­  !­  *­  %­  <­  >­  &­  |­  ^­  ~­  ?­
        operatorHead.Set('/', true);
        operatorHead.Set('=', true);
        operatorHead.Set('-', true);
        operatorHead.Set('+', true);
        operatorHead.Set('!', true);
        operatorHead.Set('*', true);
        operatorHead.Set('%', true);
        operatorHead.Set('<', true);
        operatorHead.Set('>', true);
        operatorHead.Set('&', true);
        operatorHead.Set('|', true);
        operatorHead.Set('^', true);
        operatorHead.Set('~', true);
        operatorHead.Set('?', true);

        // operator-head → U+00A1–U+00A7
        _setBitRange(ref operatorHead, 0x00A1, 0x00A7);

        // operator-head → U+00A9 or U+00AB
        operatorHead.Set((0x00A9), true);
        operatorHead.Set((0x00AB), true);

        // operator-head → U+00AC or U+00AE
        operatorHead.Set((0x00AC), true);
        operatorHead.Set((0x00AE), true);

        // operator-head → U+00A9 or U+00AB
        operatorHead.Set((0x00A9), true);
        operatorHead.Set((0x00AB), true);

        // operator-head → U+00B0–U+00B1, U+00B6, U+00BB, U+00BF, U+00D7, or U+00F7

        operatorHead.Set(0x00B0, true);
        operatorHead.Set(0x00B1, true);
        operatorHead.Set((0x00B6), true);
        operatorHead.Set((0x00BB), true);
        operatorHead.Set((0x00BF), true);
        operatorHead.Set((0x00D7), true);
        operatorHead.Set((0x00F7), true);

        // operator-head → U+2016–U+2017 or U+2020–U+2027
        operatorHead.Set(0x2016, true);
        operatorHead.Set(0x2017, true);
        _setBitRange(ref operatorHead, 0x2020, 0x2027);

        // operator-head → U+2030–U+203E
        _setBitRange(ref operatorHead, 0x2030, 0x203E);

        // operator-head → U+2041–U+2053
        _setBitRange(ref operatorHead, 0x2041, 0x2053);

        // operator-head → U+2055–U+205E
        _setBitRange(ref operatorHead, 0x2055, 0x205E);

        // operator-head → U+2190–U+23FF
        _setBitRange(ref operatorHead, 0x2190, 0x23FF);

        // operator-head → U+2500–U+2775
        _setBitRange(ref operatorHead, 0x2500, 0x2775);

        // operator-head → U+2794–U+2BFF
        _setBitRange(ref operatorHead, 0x2794, 0x2BFF);

        // operator-head → U+2E00–U+2E7F
        _setBitRange(ref operatorHead, 0x2E00, 0x2E7F);

        // operator-head → U+3001–U+3003
        _setBitRange(ref operatorHead, 0x3001, 0x3003);

        // operator-head → U+3008–U+3030
        _setBitRange(ref operatorHead, 0x3008, 0x3030);

        // operator-character → operator-head­
        operatorCharacter = (BitArray)operatorHead.Clone();

        // operator-character → U+0300–U+036F
        _setBitRange(ref operatorCharacter, 0x0300, 0x036F);

        // operator-character → U+1DC0–U+1DFF
        _setBitRange(ref operatorCharacter, 0x1DC0, 0x1DFF);

        // operator-character → U+20D0–U+20FF
        _setBitRange(ref operatorCharacter, 0x20D0, 0x20FF);

        // operator-character → U+FE00–U+FE0F
        _setBitRange(ref operatorCharacter, 0xFE00, 0xFE0F);

        // operator-character → U+FE20–U+FE2F
        _setBitRange(ref operatorCharacter, 0xFE20, 0xFE2F);

        // operator-character → U+E0100–U+E01EF
        // Java works with 16-bit unicode chars. However, it can work for targets in other languages, e.g. in Swift
        // operatorCharacter.set(0xE0100,0xE01EF+1);

        leftWS.Set(SwiftParser.WS, true);
        leftWS.Set(SwiftParser.LPAREN, true);
        leftWS.Set(SwiftParser.LBRACK, true);
        leftWS.Set(SwiftParser.LCURLY, true);
        leftWS.Set(SwiftParser.COMMA, true);
        leftWS.Set(SwiftParser.COLON, true);
        leftWS.Set(SwiftParser.SEMI, true);

        rightWS.Set(SwiftParser.WS, true);
        rightWS.Set(SwiftParser.RPAREN, true);
        rightWS.Set(SwiftParser.RBRACK, true);
        rightWS.Set(SwiftParser.RCURLY, true);
        rightWS.Set(SwiftParser.COMMA, true);
        rightWS.Set(SwiftParser.COLON, true);
        rightWS.Set(SwiftParser.SEMI, true);
        rightWS.Set(SwiftParser.LINE_COMMENT, true);
        rightWS.Set(SwiftParser.COMMENT, true);
    }

    private static void _setBitRange(ref BitArray bitArray, int firstIndex, int lastIndex)
    {
        for (var i = firstIndex; i < lastIndex; i++)
        {
            bitArray.Set(i, true);
        }
    }
    private static bool _IsCharacterFromSet(IToken token, BitArray bitArray) {
        if (token.Type == Eof) {
            return false;
        }

        String text = token.Text;
        int codePoint = text[0];

        //  Determine number of characters needed to represent the codePoint character
        if ((codePoint >= 0x10000 ? 2 : 1) != text.Length) {
            // not a single character
            return false;
        }

        return operatorCharacter.Get(codePoint);
    }

    public static bool isOperatorHead(IToken token) {
        return _IsCharacterFromSet(token, operatorHead);
    }

    public static bool isOperatorCharacter(IToken token) {
        return _IsCharacterFromSet(token, operatorCharacter);
    }

    public static bool isOpNext(ITokenStream tokens) {
        int start = tokens.Index;
        IToken lt = tokens.Get(start);
        int stop = getLastOpTokenIndex(tokens);
        if ( stop==-1 ) return false;
        // System.out.printf("isOpNext: i=%d t='%s'", start, lt.getText());
        // System.out.printf(", op='%s'\n", tokens.getText(Interval.of(start,stop)));
        return true;
    }

    /** Find stop token index of next operator; return -1 if not operator. */
    public static int getLastOpTokenIndex(ITokenStream tokens) {
        int currentTokenIndex = tokens.Index; // current on-channel lookahead token index
        IToken currentToken = tokens.Get(currentTokenIndex);

        //System.out.println("getLastOpTokenIndex: "+currentToken.getText());


        // operator → dot-operator-head­ dot-operator-characters
        if (currentToken.Type == SwiftParser.DOT && tokens.Get(currentTokenIndex + 1).Type == SwiftParser.DOT) {
            //System.out.println("DOT");

            // dot-operator
            currentTokenIndex += 2; // point at token after ".."
            currentToken = tokens.Get(currentTokenIndex);

            // dot-operator-character → .­ | operator-character­
            while (currentToken.Type == SwiftParser.DOT || isOperatorCharacter(currentToken)) {
                //System.out.println("DOT");
                currentTokenIndex++;
                currentToken = tokens.Get(currentTokenIndex);
            }

            //System.out.println("result: "+(currentTokenIndex - 1));
            return currentTokenIndex - 1;
        }

        // operator → operator-head­ operator-characters­?

        if (isOperatorHead(currentToken)) {
            //System.out.println("isOperatorHead");

            tokens.GetText(); // TODO. This line strangely fixes crash at mvn test, however, mvn compile gives me perfect working binary.
            currentToken = tokens.Get(currentTokenIndex);
            while (isOperatorCharacter(currentToken)) {
                //System.out.println("isOperatorCharacter");
                currentTokenIndex++;
                currentToken = tokens.Get(currentTokenIndex);
            }
            //System.out.println("result: "+(currentTokenIndex - 1));
            return currentTokenIndex - 1;
        } else {
            //System.out.println("result: "+(-1));
            return -1;
        }
    }

    /**
     "If an operator has whitespace around both sides or around neither side,
     it is treated as a binary operator. As an example, the + operator in a+b
     and a + b is treated as a binary operator."
     */
    public static bool isBinaryOp(ITokenStream tokens) {
        int stop = getLastOpTokenIndex(tokens);
        if (stop == -1)
        {
            return false;
        }

        int start = tokens.Index;
        IToken prevToken = tokens.Get(start-1); // includes hidden-channel tokens
        IToken nextToken = tokens.Get(stop+1);
        bool prevIsWS = isLeftOperatorWS(prevToken);
        bool nextIsWS = isRightOperatorWS(nextToken);
        bool result = prevIsWS && nextIsWS || (!prevIsWS && !nextIsWS);
        String text = tokens.GetText(Interval.Of(start, stop));
        //System.out.println("isBinaryOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
        return result;
    }

    /**
     "If an operator has whitespace on the left side only, it is treated as a
     prefix unary operator. As an example, the ++ operator in a ++b is treated
     as a prefix unary operator."
    */
    public static bool isPrefixOp(ITokenStream tokens) {
        int stop = getLastOpTokenIndex(tokens);
        if ( stop==-1 ) return false;

        int start = tokens.Index;
        IToken prevToken = tokens.Get(start-1); // includes hidden-channel tokens
        IToken nextToken = tokens.Get(stop+1);
        bool prevIsWS = isLeftOperatorWS(prevToken);
        bool nextIsWS = isRightOperatorWS(nextToken);
        bool result = prevIsWS && !nextIsWS;
        String text = tokens.GetText(Interval.Of(start, stop));
        // System.out.println("isPrefixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
        return result;
    }

    /**
     "If an operator has whitespace on the right side only, it is treated as a
     postfix unary operator. As an example, the ++ operator in a++ b is treated
     as a postfix unary operator."

     "If an operator has no whitespace on the left but is followed immediately
     by a dot (.), it is treated as a postfix unary operator. As an example,
     the ++ operator in a++.b is treated as a postfix unary operator (a++ .b
     rather than a ++ .b)."
     */
    public static bool isPostfixOp(ITokenStream tokens) {
        int stop = getLastOpTokenIndex(tokens);
        if ( stop==-1 ) return false;

        int start = tokens.Index;
        IToken prevToken = tokens.Get(start-1); // includes hidden-channel tokens
        IToken nextToken = tokens.Get(stop+1);
        bool prevIsWS = isLeftOperatorWS(prevToken);
        bool nextIsWS = isRightOperatorWS(nextToken);
        bool result =
            !prevIsWS && nextIsWS ||
            !prevIsWS && nextToken.Type==SwiftParser.DOT;
        String text = tokens.GetText(Interval.Of(start, stop));
        // System.out.println("isPostfixOp: '"+prevToken+"','"+text+"','"+nextToken+"' is "+result);
        return result;
    }

    public static bool isOperator(ITokenStream tokens, String op) {
        int stop = getLastOpTokenIndex(tokens);
        if ( stop==-1 ) return false;

        int start = tokens.Index;
        String text = tokens.GetText(Interval.Of(start, stop));
        // System.out.println("text: '"+text+"', op: '"+op+"', text.equals(op): '"+text.equals(op)+"'");

        for (int i = 0; i <= stop; i++) {
            // System.out.println("token["+i+"] = '"+tokens.getText(Interval.of(i, i))+"'");
        }

        return text.Equals(op);
    }

    public static bool isLeftOperatorWS(IToken t) {
        return leftWS.Get(t.Type);
    }

    public static bool isRightOperatorWS(IToken t) {
        return rightWS.Get(t.Type) || t.Type == SwiftParser.Eof;
    }

/*    public static bool isSeparatedStatement(ITokenStream tokens, int indexOfPreviousStatement) {
        int indexFrom = indexOfPreviousStatement - 1;
        int indexTo = tokens.Index - 1;

        // Stupid check for new line and semicolon, can be optimized
        if (indexFrom >= 0) {
            while (indexFrom >= 0 && tokens.Get(indexFrom).Channel == Lexer.Hidden) {
                indexFrom--;
            }

            //System.out.println("from: '" + tokens.getText(Interval.of(indexFrom, indexFrom))+"', "+tokens.get(indexFrom));
            //System.out.println("to: '" + tokens.getText(Interval.of(indexTo, indexTo))+"', "+tokens.get(indexTo));
            //System.out.println("in_between: '" + tokens.getText(Interval.of(indexFrom, indexTo)));

            if (tokens.GetText(Interval.Of(indexFrom, indexTo)).IndexOf("\n") >= 0
                || tokens.GetText(Interval.Of(indexFrom, indexTo)).IndexOf(";") >= 0)
            {
                return true;
            }

            return false;
        }

        return true;
    }
    
    protected bool checkPreviousTokenText(string text)
    {
        return _input.Lt(1).Text?.Equals(text) ?? false;
    }
*/

/*
    public bool isSeparatedStatement(ITokenStream tokens, int indexOfPreviousStatement) {
        // Get the token ahead of the current index.
        int possibleIndexEosToken = CurrentToken.TokenIndex - 1;

        if (possibleIndexEosToken == -1)
        {
            return true;
        }

        IToken ahead = _input.Get(possibleIndexEosToken);
        if (ahead.Channel != Lexer.Hidden)
        {
            // We're only interested in tokens on the HIDDEN channel.
            return false;
        }

        if (ahead.Type == TERMINATOR2)
        {
            // There is definitely a line terminator ahead.
            return true;
        }

        if (ahead.Type == WS2)
        {
            // Get the token ahead of the current whitespaces.
            possibleIndexEosToken = CurrentToken.TokenIndex - 2;

            if (possibleIndexEosToken == -1)
            {
                return true;
            }

            ahead = _input.Get(possibleIndexEosToken);
        }

        // Get the token's text and type.
        String text = ahead.Text;
        int type = ahead.Type;

        // Check if the token is, or contains a line terminator.
        return type == COMMENT2 && (text.Contains("\r") || text.Contains("\n")) ||
               type == TERMINATOR2;
    }
    */
}
