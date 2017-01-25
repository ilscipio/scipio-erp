package com.ilscipio.scipio.ce.build.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

/**
 * SCIPIO: Represents a graph of dependencies, support algorithm(s) to resolve them.
 * <p>
 * This saves an initial list of elements and performs the algorithm on the indexes of the elements
 * in the list, faster than hashing strings constantly.
 * <p>
 * DEV NOTE: this must have no external dependencies.
 */
@SuppressWarnings("unused")
public class DependencyGraph<T> {

    private static final int MAX_CONTAINER_PREVIEW = 500;
    
    //private final boolean strict;
    
    // external representation
    private final List<T> elements; // nodes
    //private final Map<T, List<T>> dependencies;
    
    // internal representation, using integers as nodes, which correspond to indexes in the elements list
    private final Map<T, Integer> elementIndexes;
    private final int edges[][];

    /**
     * NOTE: ArrayList recommended.
     */
    public DependencyGraph(List<T> elements, Map<T, List<T>> dependencies, boolean strict) {
        //this.strict = strict;
        this.elements = elements;
        //this.dependencies = dependencies;
        this.elementIndexes = makeElemIndexMap(elements);
        this.edges = makeEdgesRepr(elements, dependencies, this.elementIndexes, strict);
    }
    
    public DependencyGraph(List<T> elements, Map<T, List<T>> dependencies) {
        this(elements, dependencies, true);
    }
    
    public DependencyGraph(Map<T, List<T>> elementsAndDependencies, boolean strict) {
        this(new ArrayList<T>(elementsAndDependencies.keySet()), elementsAndDependencies, strict);
    }
    
    public DependencyGraph(Map<T, List<T>> elementsAndDependencies) {
        this(elementsAndDependencies, true);
    }
    
    /**
     * Creates graph from a list of strings representation, where each entry has the format:
     * <code>element=dependency1;dependency2;dependency3</code>
     * where <code>=</code> is elemDelim parameter and <code>;</code> is depDelim parameter.
     * <p>
     * A string without delimiters such as:
     * <code>element</code>
     * denotes an element with no dependencies.
     * <p>
     * The same string with equals at the end such as:
     * <code>element=</code>
     * denotes element with a dependency on the empty string value.
     * <p>
     * Spaces are not trimmed and are considered significant.
     * Empty values are allowed and considered significant, unless useEmpty and strict are both
     * set to false in which case they will be discarded where possible (unless creates an erroneous statement).
     */
    public static DependencyGraph<String> fromStringDependencyList(Collection<String> strDepGraph, String elemDelim, String depDelim, boolean strict, boolean useEmpty) {
        List<String> elements = new ArrayList<>(strDepGraph.size());
        Map<String, List<String>> dependencies = new HashMap<>();
        //int i = 0;
        for(String valEntry : strDepGraph) {
            if (!useEmpty && isEmpty(valEntry)) {
                if (strict) {
                    throw new IllegalArgumentException("Empty dependency graph entry (not allowed in strict mode)");
                } else {
                    continue; // these can just be stripped for convenience
                }
            }
            
            String[] valParts = valEntry.split(Pattern.quote(elemDelim), 2);
            
            if (valParts.length >= 2) {
                String element = valParts[0];
                String depVals = valParts[1];
                
                if (!useEmpty && isEmpty(element)) { // this is never logical
                    throw new IllegalArgumentException("Empty opening element in graph dependency list entry (not allowed in strict mode)");
                }
                
                String[] depValParts = depVals.split(Pattern.quote(depDelim));
                
                Collection<String> edges = new LinkedHashSet<>();
                for(String dep : depValParts) {
                    if (!useEmpty && isEmpty(dep)) {
                        if (strict) {
                            throw new IllegalArgumentException("Empty dependency value encountered for '" + element + "' (not allowed in strict mode)");
                        } else {
                            continue; // these can just be stripped for convenience
                        }
                    }
                    edges.add(dep);
                }
                
                elements.add(element);
                dependencies.put(element, new ArrayList<String>(edges));
            } else {
                elements.add(valEntry);
                // not necessary
                //dependencies.put(valEntry, Collections.<String> emptyList());
            }
            //i++;
        }
        return new DependencyGraph<String>(elements, dependencies, strict);
    }
    
    /**
     * Creates graph from a strings representation having the format:
     * <code>element1=dependency1;dependency2;dependency3:element2=dependency1;dependency2;dependency3</code>
     * where <code>:</code> is entryDelim parameter, <code>=</code> is elemDelim parameter and <code>;</code> is depDelim parameter.
     * <p>
     * Rules are same as {@link #fromStringDependencyList(Collection, String, String)}.
     */
    public static DependencyGraph<String> fromStringDependencyList(String strDepGraph, String entryDelim, String elemDelim, String depDelim, boolean strict, boolean useEmpty) {
        String[] entries = strDepGraph.split(Pattern.quote(entryDelim));
        return fromStringDependencyList(Arrays.asList(entries), elemDelim, depDelim, strict, useEmpty);
    }
    
    private static <T> Map<T, Integer> makeElemIndexMap(List<T> elements) {
        Map<T, Integer> map = new HashMap<>();
        int i=0;
        for(T element : elements) {
            map.put(element, i);
            i++;
        }
        return map;
    }
    
    private static <T> int[][] makeEdgesRepr(List<T> elements, Map<T, List<T>> dependencies, Map<T, Integer> elementIndexes, boolean strict) {
        // Validity check: check for duplicates (NOTE: check this even if not strict, because may lead to errors or unexpected results)
        if (new LinkedHashSet<T>(elements).size() != elements.size()) {
            throw new IllegalArgumentException("Dependency resolution graph has duplicate elements (" + makePreviewStr(elements) + ")");
        }
        
        int[][] edges = new int[elements.size()][];
        int i=0;
        for(T element : elements) {
            Collection<T> deps = dependencies.get(element);

            if (!isEmpty(deps)) {
                // Validity check: check for duplicates
                Collection<T> depsSet = new LinkedHashSet<T>(deps);
                if (deps.size() != depsSet.size()) {
                    if (strict) {
                        throw new IllegalArgumentException("'" + element.toString() + "' references duplicate dependencies");
                    } else {
                        deps = depsSet;
                    }
                }
            }
            
            int[] iDeps;
            if (deps != null) {
                iDeps = new int[deps.size()];
                int j=0;
                for(T dep : deps) {
                    Integer val = elementIndexes.get(dep);
                    if (val == null) {
                        throw new IllegalArgumentException("'" + dep.toString() + "' is referenced as a dependency but"
                                + " is not found within the list of available elements to order (" + makePreviewStr(elements) + ")");
                    }
                    iDeps[j] = val;
                    j++;
                }
            } else {
                iDeps = new int[0];
            }
            edges[i] = iDeps;
            i++;
        }
        return edges;
    }
    
    private static <T> List<T> toResolvedList(List<Integer> out, List<T> elements) {
        List<T> res = new ArrayList<>(out.size());
        for(Integer index : out) {
            res.add(elements.get(index));
        }
        return res;
    }
    
    public int getElementCount() {
        return edges.length;
    }

    /**
     * Resolves dependencies using a topological DFS search algorithm.
     * Attempts to preserve the initial node order where possible.
     */
    public List<T> getResolvedDependenciesDfs() {
        int elemCount = getElementCount();
        boolean[] resolved = new boolean[elemCount]; // NOTE: initialized to false
        boolean[] unresolved = new boolean[elemCount]; // NOTE: initialized to false
        ArrayList<Integer> out = new ArrayList<>(elemCount);
        
        for(int i = 0; i < elemCount; i++) {
            if (!resolved[i]) {
                resolveDepsDfs(i, out, resolved, unresolved);
            }
        }
        
        return toResolvedList(out, elements);
    }
    
    private void resolveDepsDfs(int node, ArrayList<Integer> out, boolean[] resolved, boolean[] unresolved) {
        unresolved[node] = true;
        for(int edge : edges[node]) {
            if (!resolved[edge]) {
                if (unresolved[edge]) {
                    throw new IllegalStateException("Circular reference detected in dependencies: '" + getElemByIndex(node).toString()
                            + "' -> '" + getElemByIndex(edge).toString() + "'");
                }
                resolveDepsDfs(edge, out, resolved, unresolved);
            }
        }
        unresolved[node] = false;
        resolved[node] = true;
        out.add(node);
    }
    
    private T getElemByIndex(int index) {
        return elements.get(index);
    }

    /**
     * Dependency functions command line main (callable from Ant).
     * <p>
     * Results are output to stdout with no other formatting, for capture and processing.
     * <p>
     * Arguments (all required except -a and -dn):
     * <pre>
     * -f=[function name]   function to run (required)
     *                      Supported:
     *                        resolve-deps-dfs
     * -do=[output delim]   output list delimiter (required)
     *                      e.g.: ";"
     * -dn=[entry delim]    entry delimiter (optional)
     *                      e.g.: "#"
     * -de=[elem delim]     element delimiter (required)
     *                      e.g.: "="
     * -dd=[dep delim]      dependency delimiter (required)
     *                      e.g.: ";"
     * -s=[true/false]      strict mode (default true)
     * -e=[true/false]      use empty string as value (default true)
     *                      if both strict and use empty are false, automatically 
     *                      strips out where possible.
     * -g=[graph repr]      graph string representation, full or entry (required)
     *                      if -dn is set, must occur only once, with complex value in the format:
     *                        element1=dependency1;dependency2#element2=dependency1;dependency2#...
     *                      where "#" is entry delimiter, "=" is element delimiter and ";" is dependency delimiter.
     *                      if -dn is ommitted, may occur as many times as needed, with values in the format:
     *                        element=dependency1;dependency2,...
     *                      WARN: TODO: only tested with -dn
     * </pre>
     */
    public static void main(String[] argsArr) {
        CmdArgs args = new CmdArgs(argsArr, Arrays.asList("-f", "-do", "-dn", "-de", "-dd", "-s", "-e", "-g"));
        
        final String[] supportedFuncs = new String[] { "resolve-deps-dfs" };
        String func = args.getSingleRequiredNonEmptyArgValue("-f");
        if ("resolve-deps-dfs".equals(func)) {
            String outDelim = args.getSingleRequiredNonEmptyArgValue("-do");
            String entryDelim = args.getFirstArgValue("-dn");
            String elemDelim = args.getSingleRequiredNonEmptyArgValue("-de");
            String depDelim = args.getSingleRequiredNonEmptyArgValue("-dd");
            boolean strict = !"false".equals(args.getFirstArgValue("-s"));
            boolean useEmpty = !"false".equals(args.getFirstArgValue("-e"));
            
            List<String> strDepGraph = args.getRequiredArgValues("-g");
            DependencyGraph<String> depGraph;
            if (!isEmpty(entryDelim)) {
                depGraph = fromStringDependencyList(strDepGraph.get(0), entryDelim, elemDelim, depDelim, strict, useEmpty);
            } else {
                depGraph = fromStringDependencyList(strDepGraph, elemDelim, depDelim, strict, useEmpty);
            }

            List<String> resolved = depGraph.getResolvedDependenciesDfs();
            
            // NOTE: must ONLY output the resolved list because this gets captured by Ant
            System.out.print(join(resolved, outDelim));
            
        } else {
            throw new IllegalArgumentException("Unrecognized function name. Supported: " + supportedFuncs.toString());
        }
    }
    
    /**
     * TODO?: factor out or replace with more formal. for now this at least avoid dependencies...
     */
    private static class CmdArgs {
        
        public static final String POS_ARG_NAME = "_POS_";
        
        private final Map<String, List<String>> args;
        private final Set<String> allowedArgNames;
        
        public CmdArgs(List<String> args, Collection<String> allowedArgNames) {
            this.allowedArgNames = (allowedArgNames instanceof Set) ? (Set<String>) allowedArgNames : new HashSet<String>(allowedArgNames);
            this.args = toArgsMap(args);
        }
        
        public CmdArgs(String[] args, Collection<String> allowedArgNames) {
            this(Arrays.asList(args), allowedArgNames);
        }

        private Map<String, List<String>> toArgsMap(List<String> args) {
            Map<String, List<String>> argsMap = new LinkedHashMap<>();
            for(String arg : args) {
                if (arg.startsWith("-")) {
                    String[] parts = arg.split(Pattern.quote("="), 2);
                    if (parts.length >= 2) {
                        appendArg(argsMap, parts[0], parts[1]);
                    } else {
                        appendArg(argsMap, parts[0], null);
                    }
                } else {
                    appendArg(argsMap, POS_ARG_NAME, arg);
                }
            }
            return argsMap;
        }
        
        private void appendArg(Map<String, List<String>> argsMap, String name, String val) {
            if (!POS_ARG_NAME.equals(name) && !allowedArgNames.contains(name)) {
                throw new IllegalArgumentException("Unrecognized parameter: " + name);
            }
            List<String> vals = argsMap.get(name);
            if (vals == null) {
                argsMap.put(name, val != null ? Arrays.asList(val) : new ArrayList<String>());
            } else if (val != null) {
                vals.add(val);
            }
        }

        public List<String> getArgValues(String name) {
            return args.get(name);
        }
        
        public List<String> getRequiredArgValues(String name) {
            List<String> vals = getArgValues(name);
            if (isEmpty(vals)) {
                throw new IllegalArgumentException("Missing required argument: " + name);
            }
            return vals;
        }
        
        public String getFirstArgValue(String name) {
            List<String> vals = getArgValues(name);
            if (!isEmpty(vals)) {
                return vals.get(0);
            }
            return null;
        }
        
        public String getSingleRequiredArgValue(String name) {
            List<String> vals = getArgValues(name);
            if (isEmpty(vals)) {
                throw new IllegalArgumentException("Missing required argument: " + name);
            }
            if (vals.size() != 1) {
                throw new IllegalArgumentException("Too many instances of argument passed: " + name);
            }
            return vals.get(0);
        }
        
        public String getSingleRequiredNonEmptyArgValue(String name) {
            String val = getSingleRequiredArgValue(name);
            if (isEmpty(val)) {
                throw new IllegalArgumentException("Missing required argument value: " + name);
            }
            return val;
        }
        
    }
    
    
    // Helpers (NOTE: duplication to avoid dependencies)
    
    private static String join(Collection<String> coll, String delim) { // (to avoid dependencies)
        if (coll.isEmpty()) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for(String str : coll) {
            sb.append(delim);
            sb.append(str);
        }
        return sb.substring(delim.length());
    }
    
    private static boolean isEmpty(String str) {
        return (str == null || str.isEmpty());
    }
    
    private static boolean isEmpty(Collection<?> coll) {
        return (coll == null || coll.isEmpty());
    }
    
    private static <T> String makePreviewStr(Collection<T> coll) {
        return makePreviewStr(new ArrayList<T>(coll).toString());
    }
    
    private static <K, V> String makePreviewStr(Map<K, V> map) {
        return makePreviewStr(new LinkedHashMap<K, V>(map).toString());
    }
    
    private static String makePreviewStr(String str) {
        String preview = str;
        if (preview.length() > MAX_CONTAINER_PREVIEW) {
            preview = preview.substring(0, MAX_CONTAINER_PREVIEW) + "...";
        }
        return preview;
    }
}
