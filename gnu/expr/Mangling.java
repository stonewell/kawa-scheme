package gnu.expr;

/** Helper methods for mangling and demangling.
 * "Mangling" is converting a user-level identifier to an internal
 * system-level name, which may be restricted in allowed characters.
 * "Demangling" is the inverse operation.
 */

public class Mangling {
    /* #ifdef Android */
    //  public static final boolean USE_SYMBOLIC = false;
    /* #else */
    public static final boolean USE_SYMBOLIC = true;
    /* #endif */

    /** Mangle a simple class or package name.
     * Does not handle qualified names.
     */
    public static String mangleClassName(String name) {
        if (USE_SYMBOLIC)
            return mangleSymbolic(name, 'C', false);
        else
            return mangleNameIfNeeded(name);
    }

    /** Mangle a possibly-qualified class name. */
    public static String mangleQualifiedName(String name) {
        if (USE_SYMBOLIC)
            return mangleSymbolic(name, 'Q', false);
        else {
            StringBuilder sbuf = new StringBuilder();
            int prev = 0;
            for (;;) {
                int dot = name.indexOf('.', prev);
                String part =
                    name.substring(prev, dot >= 0 ? dot : name.length());
                sbuf.append(mangleNameIfNeeded(part));
                if (dot < 0)
                    break;
                prev = dot + 1;
                sbuf.append('.');
            }
            return sbuf.toString();
        }
    }

    public static String mangleVariable(String name) {
        if (USE_SYMBOLIC)
            return mangleSymbolic(name, 'V', false);
        else
            return Mangling.mangleNameIfNeeded(name);
    }

    public static String mangleField(String name) {
        return Mangling.mangleNameIfNeeded(name);
    }
    public static String demangleField(String name) {
        if (USE_SYMBOLIC)
            return demangleSymbolic(name);
        else
            return demangleName(name, true);
    }

    public static String mangleMethod(String name) {
        // Don't use "symbolic" mangling.
        return mangleName(name);
    }

    public static String demangleMethod(String name) {
        // Don't use "symbolic" mangling.
        return demangleName(name, false);
    }

    public static String demangleQualifiedName(String name) {
        if (USE_SYMBOLIC)
            return Mangling.demangleSymbolic(name);
        else
            return Mangling.demangleName(name, false);
    }

    /** Mangle according to John Rose's "Symbolic Freedom in the VM".
     * <a href="https://blogs.oracle.com/jrose/entry/symbolic_freedom_in_the_vm">See this article.</a>
     * @param context One of 'C' (class name); 'Q' (qualified name, with dots);
     *   'F' (field name); 'M' (method name); 'V' (local variable name).
     * @param force True if should escape '\\' even if that is the
     *   only disallowed character.  The may cause an already-mangled name
     *   to be doubly mangled.
     */
    public static String mangleSymbolic(String name,
                                        char context, boolean force) {
        StringBuilder sbuf = null;
        int len = name.length();
        if (len == 0)
            return "\\=";
        int dangerous = 0;
        for (int i = 0; i < len; i++) {
            char ch = name.charAt(i);
            char ch2 = 0;
            switch (ch) {
            case '/':  ch2 = '|'; break;
            case '.':  if (context != 'Q') ch2 = ','; break;
            case ';':  ch2 = '?'; break;
            case '$':  ch2 = '%'; break;
            case '<':  if (context == 'M') ch2 = '^'; break;
            case '>':  if (context == 'M') ch2 = '_'; break;
            case '[':  ch2 = '{'; break;
            case ']':  ch2 = '}'; break;
            case ':':  ch2 = '!'; break;
            case '\\': ch2 = '-'; break;
            }
            if (ch2 != 0 && ch != '\\')
                dangerous++; 
            if (sbuf != null) {
                if (ch2 == 0)
                    sbuf.append(ch);
                else
                    sbuf.append('\\').append(ch2);
            } else if (ch2 != 0) {
                sbuf = new StringBuilder();
                if (i != 0)
                    sbuf.append("\\=");
                sbuf.append(name, 0, i);
                sbuf.append('\\').append(ch2);
            }
        }
        return sbuf == null || (dangerous == 0 && ! force) ? name
            : sbuf.toString();
    }

    public static String demangleName(String name, boolean reversible) {
        StringBuffer sbuf = new StringBuffer();
        int len = name.length();
        boolean mangled = false;
        boolean predicate = false;
        boolean downCaseNext = false;
        for (int i = 0;  i < len;  i++) {
            char ch = name.charAt(i);
            if (i == 0 && ch == '$' && len >= 3 && name.charAt(1) == 'N') {
                i = 1;
                mangled = true;
                continue;
            }
            if (downCaseNext && ! reversible) {
                ch = Character.toLowerCase(ch);
                downCaseNext = false;
            }
            char d;
            if (!reversible
                && ch == 'i' && i == 0 && len > 2 && name.charAt(i+1) == 's'
                && ! Character.isLowerCase(d = name.charAt(i+2))) {
                mangled = true;
                predicate = true;
                i++;
                if (Character.isUpperCase(d) || Character.isTitleCase(d)) {
                    sbuf.append(Character.toLowerCase(d));
                    i++;
                    continue;
                }
                continue;
            } else if (ch == '$' && i + 2 < len) {
                char c1 = name.charAt(i+1);
                char c2 = name.charAt(i+2);
                d = demangle2(c1, c2);
                if (d != (char)(-1)) {
                    sbuf.append(d);
                    i += 2;
                    mangled = true;
                    downCaseNext = true;
                    continue;
                } else if (c1 == 'T' && c2 == 'o' && i + 3 < len
                           && name.charAt(i+3) == '$') {
                    sbuf.append("->");
                    i += 3;
                    mangled = true;
                    downCaseNext = true;
                    continue;
                }
            } else if (! reversible && i > 1
                       && (Character.isUpperCase(ch)
                           || Character.isTitleCase(ch))
                       && (Character.isLowerCase(name.charAt(i-1)))) {
                sbuf.append('-');
                mangled = true;
                ch = Character.toLowerCase(ch);
            }
            sbuf.append(ch);
        }
        if (predicate)
            sbuf.append('?');
        return mangled ? sbuf.toString() : name;
    }

    /** Demangle a three-character mangling starting with '$'.
     * UNFINISHED!
     */
    public static char demangle2(char char1, char char2) {
        switch (char1 << 16 | char2) {
        case 'A' << 16 | 'm':  return '&';
        case 'A' << 16 | 't':  return '@';
        case 'C' << 16 | 'l':  return ':';
        case 'C' << 16 | 'm':  return ',';
        case 'D' << 16 | 'q':  return '\"';
        case 'D' << 16 | 't':  return '.';
        case 'E' << 16 | 'q':  return '=';
        case 'E' << 16 | 'x':  return '!';
        case 'G' << 16 | 'r':  return '>';
        case 'L' << 16 | 'B':  return '[';
        case 'L' << 16 | 'C':  return '{';
        case 'L' << 16 | 'P':  return '(';
        case 'L' << 16 | 's':  return '<';
        case 'M' << 16 | 'c':  return '%';
        case 'M' << 16 | 'n':  return '-';
        case 'N' << 16 | 'm':  return '#';
        case 'P' << 16 | 'c':  return '%';
        case 'P' << 16 | 'l':  return '+';
        case 'Q' << 16 | 'u':  return '?';
        case 'R' << 16 | 'B':  return ']';
        case 'R' << 16 | 'C':  return '}';
        case 'R' << 16 | 'P':  return ')';
        case 'S' << 16 | 'C':  return ';';
        case 'S' << 16 | 'l':  return '/';
        case 'S' << 16 | 'q':  return '\\';
        case 'S' << 16 | 't':  return '*';
        case 'T' << 16 | 'l':  return '~';
        case 'U' << 16 | 'p':  return '^';
        case 'V' << 16 | 'B':  return '|';
        }
        return (char) (-1);
    }

    public static String demangleSymbolic(String name) {
        int len = name.length();
        if (len < 2 || name.charAt(0) != '\\')
            return name;
        StringBuilder sbuf = new StringBuilder();
        int i = name.charAt(1) == '=' ? 2 : 0;
        while (i < len) {
            char ch = name.charAt(i);
            if (ch == '\\' && i+1 < len) {
                char ch2 = name.charAt(i+1);
                char ch1;
                switch (ch2) {
                case '|':  ch1 = '/'; break;
                case ',':  ch1 = '.'; break;
                case '?':  ch1 = ';'; break;
                case '%':  ch1 = '$'; break;
                case '^':  ch1 = '<'; break;
                case '_':  ch1 = '>'; break;
                case '{':  ch1 = '['; break;
                case '}':  ch1 = ']'; break;
                case '!':  ch1 = ':'; break;
                case '-':  ch1 = '\\'; break;
                default: ch1 = 0; break;
                }
                if (ch1 != 0)
                    sbuf.append(ch1);
                else
                    sbuf.append('\\').append(ch2);
                i += 2;
            } else {
                sbuf.append(ch);
                i += 1;
            }
        }
        return sbuf.toString();
    }

    public static String mangleName(String name) {
        return Language.mangleName(name, -1);
    }

    /** Convert a string to a safe Java identifier.
     * @param reversible if we should use an invertible mapping.
     */
    public static String mangleName(String name, boolean reversible) {
        return Language.mangleName(name, reversible ? 1 : -1);
    }

    public static String mangleNameIfNeeded(String name) {
        if (name == null || Language.isValidJavaName(name))
            return name;
        else if (USE_SYMBOLIC)
            return mangleSymbolic(name, 'F', false);
        else
            return Language.mangleName(name, 0);
    }
}
