package org.ofbiz.widget.renderer.macro;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;

/**
 * Scipio: Generalized helper class to help implement one-shot macros.
 * <p>
 * This produces a single macro invocation with the old Ofbiz sub-invocations converted
 * to map arguments, which may then contain further maps within.
 * <p>
 * This tries to factor out as much as possible from the stock Ofbiz classes to minimize
 * changes needed.
 * <p>
 * FIXME?: The code is inefficient (in particular the substitute vars kludge), but the advantage is less intrusion.
 */
class OneShotMacro {

    public static final String module = OneShotMacro.class.getName();
    
    private final boolean enabled;
    private State state;
    
    private final Map<String, OneShotMacro.Entry> macroNameMap;
    
    private final String macroName;
    
    public OneShotMacro(boolean enabled, String macroName, Map<String, Entry> macroNameMap) {
        this.enabled = enabled;
        this.macroName = macroName;
        this.macroNameMap = macroNameMap;
        this.state = new State(macroName);
    }
    
    public boolean isEnabled() {
        return enabled;
    }
    
    public void resetState() {
        state = new State(macroName);
    }
    
    /**
     * Returns true if ready to produce macro invocation - enabled and returned to root level.
     */
    public boolean isReady() {
        return (enabled && state.sb.length() > 0 && state.isRootLevel());
    }
    
    /**
     * Returns the final macro invocation string representation.
     * <p>
     * WARN: Caller should probably not modify.
     */
    public StringBuffer getBuffer() {
        return state.sb;
    }
    
    @Override
    public String toString() {
        return state.sb.toString();
    }
    
    /**
     * Transforms a legacy Ofbiz macro call into data to be appended to the one-shot buffer.
     * <p>
     * DEV NOTE: This checks macroName as a workaround to having to modify all the other
     * methods... subject to change.
     */
    public void appendData(Appendable origWriter, String macroName, Map<String, Object> macroParameters) {
        Entry entry = macroNameMap.get(macroName);
        if (entry == null) {
            throw new IllegalStateException("Scipio: unknown widget menu render entry type (" + macroName + "). Did Ofbiz source change?");
        }
        entry.appendData(state, macroParameters, origWriter);
    }
    
    static class State {
        public final StringBuffer sb = new StringBuffer();
        public final String macroName;
        private final List<LevelInfo> levelStack;
        private final Map<String, VarTextValuePair> substituteVarMap = new HashMap<String, VarTextValuePair>();
        private int subsituteVarCounter = 0;
        
        public State(String macroName) {
            this.macroName = macroName;
            levelStack = new ArrayList<LevelInfo>();
            levelStack.add(new LevelInfo());
        }
        
        public LevelInfo getLevelInfo() {
            return levelStack.get(levelStack.size() - 1);
        }
        
        public void pushLevelInfo(LevelInfo levelInfo) {
            levelStack.add(levelInfo);
        }
        
        public LevelInfo popLevelInfo() {
            return levelStack.remove(levelStack.size() - 1);
        }
        
        public boolean isRootLevel() {
            return (levelStack.size() <= 1);
        }
        
        public boolean isParentRootLevel() {
            return (levelStack.size() == 2);
        }
        
        public boolean isHasItems() {
            return (levelStack.get(levelStack.size() - 1)).hasItems;
        }
        
        public boolean isList() {
            return (levelStack.get(levelStack.size() - 1)).currListVarName != null;
        }
        
        public String addSubstituteVarText(String varName, String text) {
            final String identifier = "##SCIPIO_SUBS_" + subsituteVarCounter;
            subsituteVarCounter++;
            substituteVarMap.put(identifier, new VarTextValuePair(varName, text));
            return identifier;
        }
        
        public VarTextValuePair getSubsituteTextForId(String identifier) {
            return substituteVarMap.get(identifier);
        }
    }
    
    private static class VarTextValuePair {
        public final String varName;
        public final String text;
        public VarTextValuePair(String varName, String text) {
            super();
            this.varName = varName;
            this.text = text;
        }
    }
    

    static class LevelInfo {
        public boolean hasItems;    // needed for proper adding of commas
        public String currListVarName;  // records the var name of the current open list, if any

        public LevelInfo(boolean hasItems, String currListVarName) {
            this.hasItems = hasItems;
            this.currListVarName = currListVarName;
        }
        public LevelInfo(boolean hasItems) {
            this.hasItems = hasItems;
            this.currListVarName = null;
        }
        public LevelInfo() {
            this.hasItems = false;
            this.currListVarName = null;
        }
    }
    
    public static enum VarType {
        LIST,
        SINGLE
    }

    /**
     * One-shot render entry capture class.
     * Corresponds to one traditional Ofbiz macro invocation.
     */
    public static abstract class Entry {

        protected final VarType varType;
        protected final String varName;
        
        public Entry(VarType varType, String varName) {
            this.varType = varType;
            this.varName = varName;
        }
        public Entry() {
            this.varType = null;
            this.varName = null;
        }

        public abstract void appendData(OneShotMacro.State state, Map<String, Object> macroParameters, Appendable origWriter);
        
    }
    
    /**
     * Translation for "open" sub-macro invocations (with nested content).
     * <p>
     * Automatically detects when it's the top-level invocation.
     */
    public static class BeginEntry extends Entry {
        public BeginEntry(VarType varType, String varName) {
            super(varType, varName);
        }
    
        @Override
        public void appendData(State state, Map<String, Object> macroParameters, Appendable origWriter) {
            StringBuffer sb = state.sb;
            if (state.isRootLevel()) {
                sb.append("<@");
                sb.append(state.macroName);
                boolean hasItems = appendMacroArgs(sb, state, macroParameters);
                state.pushLevelInfo(new LevelInfo(hasItems));
            }
            else {
                checkListAppendArgName(state, this.varType, this.varName);
                boolean hasItems = appendMacroMapArg(state.sb, state, macroParameters, false);
                state.pushLevelInfo(new LevelInfo(hasItems));
            }
        }
    }

    /**
     * Translation for "close" sub-macro invocations.
     * <p>
     * Automatically detects when it's the last invocation (in normal cases).
     */
    public static class EndEntry extends Entry {
        @Override
        public void appendData(State state, Map<String, Object> macroParameters, Appendable origWriter) {
            // if a list is open, close it
            if (state.isList()) {
                appendListClose(state, this.varType, this.varName);
            }
            // pop this elem's level info (in addition to list above)
            state.popLevelInfo();
            if (state.isRootLevel()) {
                state.sb.append("/>");
            }
            else {
                state.sb.append("}");
                // we printed an item, so know previous row had items
                state.getLevelInfo().hasItems = true;
            }
        }
    }

    /**
     * Translation for single macro invocations (no nested content)
     */
    public static class SingleEntry extends Entry {
        public SingleEntry(VarType varType, String varName) {
            super(varType, varName);
        }
    
        @Override
        public void appendData(State state, Map<String, Object> macroParameters, Appendable origWriter) {
            if (origWriter instanceof StringWriter) {
                // WARNING: SPECIAL CASE: here ofbiz was trying to render to a local StringWriter to pass
                // as a variable to another macro. use a big kludge to get around, using an identifier, which
                // later we will substitute with our own variable and map data.
                StringBuffer sblocal = new StringBuffer();
                appendMacroMapArg(sblocal, state, macroParameters, true);
                String substituteId = state.addSubstituteVarText(this.varName, sblocal.toString());
                // print to the orig writer in this case
                // we print a special ID which we later scan parameter values for and substitute
                try {
                    origWriter.append(substituteId);
                } catch (IOException e) {
                    Debug.logError(e, module);
                }
            }
            else {
                checkListAppendArgName(state, this.varType, this.varName);
                appendMacroMapArg(state.sb, state, macroParameters, true);
                state.getLevelInfo().hasItems = true;
            }
        }
    }

    static boolean appendMacroArgs(StringBuffer sb, State state, Map<String, Object> args) {
        boolean hasItems = false;
        if (args != null) {
            for (Map.Entry<String, Object> parameter : args.entrySet()) {
                sb.append(' ');
                String name = parameter.getKey();
                Object value = parameter.getValue();
                
                // WARNING: SPECIAL CASE for substitute vars...
                if ((value instanceof String) && ((String) value).startsWith("##") && state.getSubsituteTextForId((String) value) != null) {
                    VarTextValuePair substituteVar = state.getSubsituteTextForId((String) value);
                    name = substituteVar.varName;
                    value = substituteVar.text;
                    sb.append(name);
                    sb.append('=');
                    sb.append(value);
                }
                else {
                    sb.append(name);
                    sb.append("=");
                    if (value instanceof String) {
                        sb.append('"');
                        sb.append(((String) value).replaceAll("\"", "\\\\\""));
                        sb.append('"');
                    } else {
                        sb.append(value);
                    }
                }
                hasItems = true;
            }
        }
        return hasItems;
    }
    
    
    static boolean appendMacroMapArg(StringBuffer sb, State state, Map<String, Object> map, boolean close) {
        sb.append('{');
        boolean hasItems = false;
        if (map != null) {
            for (Map.Entry<String, Object> parameter : map.entrySet()) {
                if (hasItems) {
                    sb.append(',');
                }
                String name = parameter.getKey();
                Object value = parameter.getValue();
                
                // WARNING: SPECIAL CASE for substitute vars...
                if ((value instanceof String) && ((String) value).startsWith("##") && state.getSubsituteTextForId((String) value) != null) {
                    VarTextValuePair substituteVar = state.getSubsituteTextForId((String) value);
                    name = substituteVar.varName;
                    value = substituteVar.text;
                    sb.append('"');
                    sb.append(name);
                    sb.append('"');
                    sb.append(':');
                    sb.append(value);
                }
                else {
                    sb.append('"');
                    sb.append(name);
                    sb.append('"');
                    sb.append(':');
                    if (value instanceof String) {
                        sb.append('"');
                        sb.append(((String) value).replaceAll("\"", "\\\\\""));
                        sb.append('"');
                    } else {
                        sb.append(value);
                    }
                }
                hasItems = true;
            }
        }
        if (close) {
            sb.append('}');
        }
        return hasItems;
    }

    static void checkListAppendArgName(State state, VarType varType, String varName) {
        if (state.isList()) {
            if (varType == VarType.LIST) {
                if (state.getLevelInfo().currListVarName.equals(varName)) {
                    // adding to same list, so only append a comma
                    if (state.isHasItems()) {
                        state.sb.append(',');
                    }
                }
                else {
                    // Different var name, so make a new list...
                    appendListClose(state, varType, varName);
                    appendArgName(state, varType, varName);
                    appendListOpen(state, varType, varName);
                }
            }
            else {
                // Incoming isn't a list, so close list...
                appendListClose(state, varType, varName);
                appendArgName(state, varType, varName);
            }
        }
        else {
            appendArgName(state, varType, varName);
            if (varType == VarType.LIST) {
                appendListOpen(state, varType, varName);
            }
        }
    }
    
    static void appendArgName(State state, VarType varType, String varName) {
        StringBuffer sb = state.sb;
        if (state.isParentRootLevel()) {
            // Still in top-level macro, so use "=" syntax
            sb.append(' ');
            sb.append(varName);
            sb.append('=');
        }
        else {
            // We're down in a map, so use map syntax
            if (state.isHasItems()) {
                sb.append(',');
            }
            sb.append('\"');
            sb.append(varName);
            sb.append("\":");
        }
    }
    
    static void appendListOpen(State state, VarType varType, String varName) {
        state.sb.append("[");
        state.pushLevelInfo(new LevelInfo(false, varName));
    }
    
    static void appendListClose(State state, VarType varType, String varName) {
        state.sb.append("]");
        state.popLevelInfo();
    }
    
}
