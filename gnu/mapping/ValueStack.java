// Copyright (c) 2001, 2015  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.lists.*;

class ValueStack extends TreeList {
    int gapStartOnPush;
    Consumer consumerOnPush = this;
    int oindexOnPush;
    /** Most recent argument to writeObject, assumming no intervining writes,
     * If writeObject is the most recent write, lastObject==this.
     */
    Object lastObject = this;

    public void clear() {
        super.clear();
        lastObject = this;
    }

    void push() {
        if (lastObject != this) {
            super.writeObject(lastObject);
            lastObject = this;
        }
        int oindex = find(consumerOnPush);
        writeIntForce32(oindex);
        writeIntForce32(gapStartOnPush);
    }

    void pop(int saved) {
        gapStartOnPush = getIntN(saved-2);
        int oindex = getIntN(saved-5);
        consumerOnPush = (Consumer) objects[oindex];
        objects[oindex] = null;
        oindexOnPush = oindex;
        gapStart = saved-6;
    }

    public void pushArgState(CallContext ctx) {
        int saveGap = gapStart;
        writeIntForce32(ctx.consumerOnPushArgState);
        writeIntForce32(ctx.next);
        writeIntForce32(ctx.count);
        writeIntForce32(ctx.firstKeyword);
        writeIntForce32(ctx.numKeywords);
        writeIntForce32(ctx.nextKeyword);
        writeIntForce32(ctx.matchState);
        ctx.consumerOnPushArgState = saveGap;
        int ogrow = ctx.count+2;
        reserveObjects(ogrow);
        System.arraycopy(ctx.values, 0, objects, oindex,
                         ctx.count);
        // Should we save applyMethod and/or proc fields?
        objects[oindex+ctx.count] = ctx.keywords;
        objects[oindex+ctx.count+1] = ctx.sortedKeywords;
        oindex += ogrow;
        ctx.next = 0;
        ctx.count = 0;
        ctx.firstKeyword = 0;
        ctx.numKeywords = 0;
        ctx.nextKeyword = 0;
        ctx.matchState = 0; // ???
        ctx.keywords = null;
        ctx.sortedKeywords = null;
    }
    public void popArgState(CallContext ctx) {
        int start = ctx.consumerOnPushArgState;
        ctx.consumerOnPushArgState = getIntN(start+1);
        ctx.next = getIntN(start+4);
        ctx.count = getIntN(start+7);
        ctx.firstKeyword = getIntN(start+10);
        ctx.numKeywords = getIntN(start+13);
        ctx.nextKeyword = getIntN(start+16);
        ctx.matchState = getIntN(start+19);
        gapStart = start;
        int ogrow = ctx.count+2;
        oindex -= ogrow;
        System.arraycopy(objects, oindex,
                         ctx.values, 0, ctx.count);
        ctx.keywords = (String[]) objects[oindex+ctx.count];
        ctx.sortedKeywords = (short[]) objects[oindex+ctx.count+1];
        for (int i = 0;  i < ogrow; i++)
            objects[oindex+1] = null;
    }

    Object getValue() {
        Object last = lastObject;
        if (gapStart == gapStartOnPush) {
            return last == this ? Values.empty : last;
        }
        int next = nextDataIndex(gapStartOnPush);
        if (next == gapStart && last == this)
            return getPosNext(gapStartOnPush << 1); // Singleton value
        Values.FromTreeList vals = new Values.FromTreeList();
        super.consumeIRange(gapStartOnPush, gapStart, vals);
        if (lastObject != this)
            vals.writeObject(lastObject);
        return vals;
    }

    @Override
    public void ensureSpace(int needed) {
        super.ensureSpace(needed+3);
        if (lastObject != this) {
            super.writeObject(lastObject);
            lastObject = this;
        }
    }

    @Override
    public void writeObject(Object v) {
        if (lastObject != this)
            super.writeObject(lastObject);
        lastObject = v;
    }
}
