// Copyright (c) 1999, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;

/** A collection of (zero or more) SourceErrors.
 * Has a "current line number" which clients can use as the default line
 * number, or clients can explicitly provide a line number.
 * Does not handle localization of messages.
 *
 * Similar functionality as JAVA6's javax.tools.DiagnosticCollector.
 */

public class SourceMessages extends SourceLocator.Simple {
    // Number of errors (not counting warnings).  A value of 1000 is "fatal".
    private int errorCount = 0;

    /** The first error or warning in a linked list. */
    SourceError firstError;
    /** The last error or warning in a linked list. */
    SourceError lastError;

    public SourceError getErrors() { return firstError; }

    SourceLocator locator;

    public static boolean stripDirectoriesDefault = false;
    public boolean stripDirectories = stripDirectoriesDefault;

    /** If true, print out stack trace with any warning. */
    public static boolean debugStackTraceOnWarning = false;

    /** If true, print out stack trace with any error. */
    public static boolean debugStackTraceOnError = false;


    /** Return true iff errors (not warnings) have been seen. */
    public boolean seenErrors() { return errorCount > 0; }

    public boolean seenErrorsOrWarnings() { return firstError != null; }

    /** Get the number of errors (not counting warnings). */
    public int getErrorCount() { return errorCount; }

    /** Get number of diagnostics whose severity is one of the characters in the argument. */
    public int getCount (String severities) {
        int count = 0;
        for (SourceError err = firstError; err != null;  err = err.next)
        {
            if (severities.indexOf(err.severity) >= 0)
                count++;
        }
        return count;
    }

    /** Clear the error count (only). */
    public void clearErrors() { errorCount = 0; }

    /** Clear the contained errors and warnings. */
    public void clear() {
        firstError = lastError = null;
        errorCount = 0;
    }

    // The last SourceError with a *differnt* filename than prev has.
    SourceError lastPrevFilename = null;

    /** True if we should sort messages by line number. */
    public boolean sortMessages;

    /** Link in an error. */
    public void error(SourceError error) {
        if (error.severity == 'f')
            errorCount = 1000;
        else if (error.severity != 'w' && error.severity != 'i')
            errorCount++;
        if (error.fakeException == null
            && ((SourceMessages.debugStackTraceOnError
                 && (error.severity == 'e' || error.severity == 'f'))
                || (SourceMessages.debugStackTraceOnWarning
                    && error.severity == 'w'))) {
            error.fakeException = new Throwable();
        }

        // Insert the next error so that line numbers are increasing.
        if (lastError != null && lastError.filename != null
            && ! lastError.filename.equals(error.filename))
            lastPrevFilename = lastError;
        SourceError prev = lastPrevFilename;
        if (! sortMessages || error.severity == 'f')
            prev = lastError;
        else {
            for (;;) {
                SourceError next;
                if (prev == null)
                    next = firstError;
                else
                    next = prev.next;
                if (next == null)
                    break;
                int errline = error.getStartLine();
                int nextline = next.getStartLine();
                if (errline != 0 && nextline != 0) {
                    if (errline < nextline)
                        break;
                    if (errline == nextline) {
                        int errcol = error.getStartColumn();
                        int nextcol = next.getStartColumn();
                        if (errcol > 0 && nextcol > 0
                            && errcol < nextcol)
                            break;
                    }
                }
                prev = next;
            }
        }
        if (prev == null) {
            error.next = firstError;
            firstError = error;
        } else {
            error.next = prev.next;
            prev.next = error;
        }
        if (prev == lastError) 
            lastError = error;
    }

    /** Record a new error.
     * @param severity is the seriousness of the error
     *  - one of 'w' (for warning), 'e' (for error), or 'f' (for fatal error)
     * @param filename the name or URL of the file containing the error
     * @param line the (1-origin) line number or 0 if unknown
     * @param column the (1-origin) column number or 0 if unknown
     * @param message the error message
     */
    public void error(char severity, String filename, int line, int column,
                      String message) {
        error(new SourceError(severity, filename, line, column, message));
    }

    public void error(char severity, SourceLocator location, String message) {
        error(new SourceError(severity, location, message));
    }

    public void error(char severity, String filename, int line, int column,
                      String message, String code) {
        SourceError err = new SourceError(severity, filename, line, column,
                                          message);
        err.code = code;
        error(err);
    }

    public void error(char severity, SourceLocator location,
                      String message, String code) {
        SourceError err = new SourceError(severity, location,  message);
        err.code = code;
        error(err);
    }

    /** Record a new error at the current default source file location.
     * @param severity is the seriousness of the error
     *  - one of 'w' (for warning), 'e' (for error), or 'f' (for fatal error)
     * @param message the error message
     */
    public void error(char severity, String message) {
        error(new SourceError(severity, this, message));
    }

    public void error(char severity, String message, Throwable exception) {
        SourceError err = new SourceError(severity, this, message);
        err.fakeException = exception;
        error(err);
    }

    public void error(char severity, String message, String code) {
        SourceError err = new SourceError(severity, this, message);
        err.code = code;
        error(err);
    }

    /** Print all the error messages to an Appendable. */
    public void printAll(Appendable out, int max) {
        int errCount = getCount("ef");
        int wrnCount = getCount("iw");
        int errLimit = max >= 0 && errCount > max ? max : errCount;
        int wrnLimit = max >= 0 && errCount + wrnCount > max ? max - errLimit
            : wrnCount;
        int skippedErrors = 0;
        int skippedWarnings = 0;
        int skippedInfo = 0;
        for (SourceError err = firstError;  err != null;  err = err.next) {
            if (err.severity == 'e' && --errLimit < 0)
                skippedErrors++;
            else if (err.severity == 'w' && --wrnLimit < 0)
                skippedWarnings++;
            else if (err.severity == 'i' && --wrnLimit < 0)
                skippedInfo++;
            else
                err.println(out, stripDirectories);
        }
        if (skippedErrors + skippedWarnings + skippedInfo > 0) {
            SourceError err =
                new SourceError('i', firstError.getFileName(), 0, 0,
                                "skipped "+skippedErrors+" errors, "
                                +skippedWarnings+" warnings, "
                                +skippedInfo+" notes");
            err.println(out, stripDirectories);
        }
    }

    /** Convert this to a String containing the recorded errors.
     * @param max the maximum number of error error to list
     * @return a String with one '\n'-terminated line per recorded error
     */
    public String toString(int max) {
        if (firstError == null)
            return null;
        StringBuilder buffer = new StringBuilder();
        for (SourceError err = firstError;
             err != null && --max >= 0;  err = err.next) {
            err.appendTo(buffer, stripDirectories, "\n");
        }
        return buffer.toString();
    }

    /** Checks if an error was seen; if so, prints and clears the messages.
     * @param out where to write the error message to
     * @param max maximum number of messages to print (can be 0)
     */
    public boolean checkErrors(Appendable out, int max) {
        if (firstError != null) {
            printAll(out, max);
            firstError = lastError = null;
            int saveCount = errorCount;
            errorCount = 0;
            return saveCount > 0;
        }
        return false;
    }

    /** Links our location to the one give. */
    public final void setSourceLocator(SourceLocator locator) {
        this.locator = locator == this ? null : locator;
    }

    public final SourceLocator swapSourceLocator(SourceLocator locator) {
        SourceLocator save = this.locator;
        this.locator = locator;
        return save;
    }

    /** Copies the current position of locator. */
    public final void setLocation(SourceLocator locator) {
        this.locator = null;
        super.setLocation(locator);
    }

    public String getPublicId() {
        return locator == null ? super.getPublicId() : locator.getPublicId();
    }
    public String getSystemId() {
        return locator == null ? super.getSystemId() : locator.getSystemId();
    }

    public boolean isStableSourceLocation() { return false; }

    /** The default filename to use for a new error. */
    public final String getFileName() {
        return locator == null ? super.getFileName() : locator.getFileName();
    }

    /** The default line number to use for a new error. */
    public final int getLineNumber() {
        return locator == null ? super.getLineNumber() : locator.getLineNumber();
    }

    /** The default column number to use for a new error. */
    public final int getColumnNumber() {
        return locator == null ? super.getColumnNumber() : locator.getColumnNumber();
    }
    public int getStartLine() {
        return locator == null ? super.getStartLine() : locator.getStartLine();
    }
    public int getStartColumn() {
        return locator == null ? super.getStartColumn() : locator.getStartColumn();
    }
    public int getEndLine() {
        return locator == null ? super.getEndLine() : locator.getEndLine();
    }
    public int getEndColumn() {
        return locator == null ? super.getEndColumn() : locator.getEndColumn();
    }

    /** Set the default column number to use for a new error. */
    public void setColumn(int column) { setLine(getLineNumber(), column); }
}
