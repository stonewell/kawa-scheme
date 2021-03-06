package gnu.kawa.io;

import java.io.*;
import java.util.List;
import gnu.expr.CommandCompleter;
import gnu.expr.Compilation;
import gnu.expr.Language;
import gnu.text.Lexer;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import org.jline.reader.Candidate;
import org.jline.reader.Completer;
import org.jline.reader.EndOfFileException;
import org.jline.reader.EOFError;
import org.jline.reader.LineReader;
import org.jline.reader.LineReaderBuilder;
import org.jline.reader.ParsedLine;
import org.jline.reader.Parser;
import org.jline.reader.Parser.ParseContext;
import org.jline.reader.UserInterruptException;
import org.jline.reader.SyntaxError;
import org.jline.reader.impl.DefaultParser;
import org.jline.terminal.Size;
import org.jline.terminal.Terminal;
import org.jline.terminal.TerminalBuilder;
import org.jline.terminal.impl.ExternalTerminal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/** A variation of TtyInPort that uses the JLine library for input editing. */

public class JLineInPort extends TtyInPort
    implements Completer, Parser
{
    LineReader jlreader;
    org.jline.terminal.Terminal terminal;
    String prompt;
    SourceMessages messages;
    String stringRest;
    /** Remaining available characters in stringRest. */
    private int charsRest;
    Language language;

    public JLineInPort(InputStream in, Path name, OutPort tie)
        throws java.io.IOException {
        this(in, name, tie, TerminalBuilder.terminal());
    }
    private static Terminal makeTerminal(InputStream in, OutputStream out) throws IOException {
          Terminal terminal = new ExternalTerminal("Kawa", "xterm-256color",
                                                   in, out,
                                                   StandardCharsets.UTF_8);
          terminal.getAttributes().setOutputFlag(org.jline.terminal.Attributes.OutputFlag.ONLCR, true);
          terminal.getAttributes().setOutputFlag(org.jline.terminal.Attributes.OutputFlag.OPOST, true);
          return terminal;
    }

    public JLineInPort(InputStream in, Path name, OutputStream out, OutPort tie)
        throws java.io.IOException {
        this(in, name, tie, makeTerminal(in, out));
    }
    public JLineInPort(InputStream in, Path name, OutPort tie, Terminal terminal)
        throws java.io.IOException {
        super(in, name, tie);
        jlreader = LineReaderBuilder.builder()
            .terminal(terminal)
            .completer(this)
            .parser(this)
            .build();
        if (CheckConsole.useJLineMouse() > 0)
            jlreader.setOpt(LineReader.Option.MOUSE);
        this.terminal = terminal;
    }

    @Override
    public void setInDomTerm(boolean v) {
        super.setInDomTerm(v);
        if (v)
            jlreader.setOpt(LineReader.Option.DELAY_LINE_WRAP);
    }

    public ParsedLine parse(String line, int cursor,
                            ParseContext context) throws SyntaxError {
        if (context == ParseContext.COMPLETE)
            return parseForComplete(line, cursor);
        CharArrayInPort cin = CharArrayInPort.make(line, "\n");
        cin.setLineNumber(this.getLineNumber());
        cin.setPath(this.getPath());
        if (language == null)
            return new KawaParsedLine(this, line, cursor);
        try {
            Lexer lexer = language.getLexer(cin, this.messages);
            lexer.setInteractive(true);
            Compilation comp =
                language.parse(lexer,
                               Language.PARSE_FOR_EVAL|Language.PARSE_INTERACTIVE_MODULE,
                               null);
            if (comp == null)
                throw new EndOfFileException();
            if (comp.getState() == Compilation.ERROR_SEEN && cin.eofSeen()) {
                messages.clear();
                throw new EOFError(-1, -1, "unexpected end-of-file", "");
            }
            return new KawaParsedLine(this, line, cursor, comp);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    ParsedLine parseForComplete(String line, int cursor)
        throws SyntaxError {
        int buflen = line.length();
        char[] tbuf = new char[buflen + 1];
        line.getChars(0, cursor, tbuf, 0);
        tbuf[cursor] = CommandCompleter.COMPLETE_REQUEST;
        line.getChars(cursor, buflen, tbuf, cursor+1);
        CharArrayInPort cin = new CharArrayInPort(tbuf);
        try {
            SourceMessages messages = new SourceMessages();
            Lexer lexer = language.getLexer(cin, messages);
            lexer.setInteractive(true);
            lexer.setTentative(true);
            Compilation comp =
                language.parse(lexer,
                               Language.PARSE_FOR_EVAL|Language.PARSE_INTERACTIVE_MODULE,
                               null);
            language.resolve(comp);
            return new KawaParsedLine(this, line, cursor, comp);
        } catch (SyntaxException ex) {
            if (cin.eofSeen())
                throw new EOFError(-1, -1, "unexpected end-of-file", "");
            throw ex;
        } catch (CommandCompleter ex) {
            return new KawaParsedLine(this, line, cursor, ex);
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void complete(LineReader reader, final ParsedLine commandLine,
                         List<Candidate> candidates) {
        KawaParsedLine kline = (KawaParsedLine) commandLine;
        if (kline.ex != null) {
            CommandCompleter ex = kline.ex;
            java.util.Collections.sort(ex.candidates);
            kline.word = ex.word;
            kline.wordCursor = ex.wordCursor;
            for (CharSequence cstr : ex.candidates) {
                String str = cstr.toString();
                candidates.add(new Candidate(str, str,
                                             null, null, null, null, true));
            }
        }
    }

    @Override
    protected int fill(int len) throws java.io.IOException {
        String line;
        int count;
        if (charsRest > 0)
            line = stringRest;
        else {
            try {
                jlreader.setVariable(LineReader.LINE_OFFSET,
                                     getLineNumber()+1);
                line = jlreader.readLine(prompt);
            } catch (UserInterruptException ex) {
                return -1;
            } catch (EndOfFileException ex) {
                promptEmitted = false;  // Disable redundant newline.
                return -1;
            }
            if (line == null)
                return -1;
            charsRest = line.length();
        }
        int start = line.length()-charsRest;
        if (charsRest < len) {
            line.getChars(start, line.length(), buffer, pos);
            buffer[pos+charsRest] = '\n';
            count = charsRest + 1;
            charsRest = 0;
            stringRest = null;
        } else {
            line.getChars(start, start+len, buffer, pos);
            stringRest = line;
            charsRest -= len;
            count = len;
        }
        afterFill(count);
        return count;
    }

    @Override
    public String promptTemplate1() {
        return maybeColorizePrompt(super.promptTemplate1());
    }

    @Override
    public String promptTemplate2() {
        return maybeColorizePrompt(super.promptTemplate2());
    }

    public String maybeColorizePrompt(String prompt) {
        if (prompt.indexOf('\033') < 0 && prompt.indexOf("%{") < 0)
            prompt = "\033[48;5;194m" + prompt + "\033[0m";
        return prompt;
    }

    @Override
    public void emitPrompt(String prompt) throws java.io.IOException {
        this.prompt = prompt;
    }

    @Override
    public String expandPrompt(String pattern, int padToWidth, int line,
                               String message, int[] width) {
        return pattern;
    }
    public void setSize(int ncols, int nrows) {
        Terminal term = terminal;
        if (term != null)
            term.setSize(new Size(ncols, nrows));
    }

    public static class KawaParsedLine implements ParsedLine {
        JLineInPort inp;
        Compilation comp;
        String source;
        int cursor;
        String word;
        int wordCursor;
        CommandCompleter ex;

        public KawaParsedLine(JLineInPort inp, String source, int cursor) {
            this.inp = inp;
            this.source = source;
            this.cursor = cursor;
            this.word = "";
        }

         public KawaParsedLine(JLineInPort inp, String source, int cursor, Compilation comp) {
            this.inp = inp;
            this.comp = comp;
            this.source = source;
            this.cursor = cursor;
            this.word = "";
        }

        public KawaParsedLine(JLineInPort inp, String source, int cursor, CommandCompleter ex) {
            this.inp = inp;
            this.comp = ex.getCompilation();
            this.source = source;
            this.cursor = cursor;
            this.ex = ex;
        }

        // This method is called using reflection
        public static Compilation parse(Language language, Lexer lexer)
            throws java.io.IOException {
	    int opts = Language.PARSE_FOR_EVAL|Language.PARSE_ONE_LINE|Language.PARSE_INTERACTIVE_MODULE;
            JLineInPort inp = (JLineInPort) lexer.getPort();
            if (inp.tie != null)
                inp.tie.freshLine();
            int line = inp.getLineNumber() + 1;
            Object p = null;
            char saveState = inp.getReadState();
            inp.readState = ' ';
            try {
                if (inp.prompter != null)
                    p = inp.prompter.apply1(inp);
            } catch (Throwable ex) {
            }
            String prompt = p == null ? "["+line+"] " : p.toString();
            inp.prompt = prompt;
            LineReader jlreader = inp.jlreader;
            jlreader.setVariable(LineReader.LINE_OFFSET, line);
            String pattern2 = inp.promptTemplate2();
            jlreader.setVariable(LineReader.SECONDARY_PROMPT_PATTERN,
                                 pattern2);
            inp.readState = saveState;
            inp.messages = lexer.getMessages();
            Language saveLanguage = inp.language; // Normally null
            inp.language = language;
            try {
                jlreader.readLine(inp.prompt);
                if (inp.tie != null)
                    inp.tie.setColumnNumber(0);
                KawaParsedLine parsedLine = (KawaParsedLine) jlreader.getParsedLine();
                inp.setLineNumber(line - 1 + parsedLine.lineCount());
                return parsedLine.comp;
            } catch (org.jline.reader.EndOfFileException ex) {
                return null;
            }
            finally {
                inp.language = saveLanguage;
            }
            
        }
        public String word() {
            return word;
        }

        public int wordCursor() {
            return wordCursor;
        }

        public int wordIndex() {
            return 0;
        }

        public List<String> words() {
            return null;
        }

        public String line() {
            return source;
        }

        public int lineCount() {
            int n = 1;
            for (int i = 0; (i = source.indexOf('\n', i) + 1) > 0; )
                n++;
            return n;
        }

        public int cursor() {
            return cursor;
        }
    }
}
