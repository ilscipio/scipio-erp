package org.ofbiz.base.util.collections;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
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
public class DependencyGraph<T> {

    // external representation
    private final List<T> elements; // nodes
    //private final Map<T, List<T>> dependencies;
    
    // internal representation, using integers as nodes, which correspond to indexes in the elements list
    private final Map<T, Integer> elementIndexes;
    private final int edges[][];

    /**
     * NOTE: ArrayList recommended.
     */
    public DependencyGraph(List<T> elements, Map<T, List<T>> dependencies) {
        this.elements = elements;
        //this.dependencies = dependencies;
        this.elementIndexes = makeElemIndexMap(elements);
        this.edges = makeEdgesRepr(elements, dependencies, this.elementIndexes);
    }
    
    public DependencyGraph(Map<T, List<T>> elementsAndDependencies) {
        this(new ArrayList<T>(elementsAndDependencies.keySet()), elementsAndDependencies);
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
     * Empty values are allowed and considered significant.
     */
    public static DependencyGraph<String> fromStringDependencyList(Collection<String> strDepGraph, String elemDelim, String depDelim) {
        List<String> elements = new ArrayList<>(strDepGraph.size());
        Map<String, List<String>> dependencies = new HashMap<>();
        //int i = 0;
        for(String valEntry : strDepGraph) {
            String[] valParts = valEntry.split(Pattern.quote(elemDelim), 2);
            if (valParts.length >= 2) {
                String element = valParts[0];
                String depVals = valParts[1];
                
                String[] depValParts = depVals.split(Pattern.quote(depDelim));
                Collection<String> edges = new LinkedHashSet<>();
                for(String dep : depValParts) {
                    edges.add(dep);
                }
                
                elements.add(element);
                dependencies.put(valEntry, new ArrayList<String>(edges));
            } else {
                elements.add(valEntry);
                // not necessary
                //dependencies.put(valEntry, Collections.<String> emptyList());
            }
            //i++;
        }
        return new DependencyGraph<String>(elements, dependencies);
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
    
    private static <T> int[][] makeEdgesRepr(List<T> elements, Map<T, List<T>> dependencies, Map<T, Integer> elementIndexes) {
        // Validity check: check for duplicates
        if (new LinkedHashSet<T>(elements).size() != elements.size()) {
            throw new IllegalArgumentException("Dependency graph has duplicate elements");
        }
        
        int[][] edges = new int[elements.size()][];
        int i=0;
        for(T element : elements) {
            Collection<T> deps = dependencies.get(element);

            // Validity check: check for duplicates
            if (deps != null && !deps.isEmpty() && deps.size() != new LinkedHashSet<T>(deps).size()) {
                throw new IllegalArgumentException("'" + element.toString() + "' contains duplicate dependencies");
            }
            
            int[] iDeps;
            if (deps != null) {
                iDeps = new int[deps.size()];
                int j=0;
                for(T dep : deps) {
                    Integer val = elementIndexes.get(dep);
                    if (val == null) {
                        throw new IllegalArgumentException("Element dependencies contain an element not"
                                + " listed in the elements list (" + dep.toString() + ")");
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
                    throw new IllegalStateException("Circular reference detected for '" + getElemByIndex(node).toString()
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
     * main - command line (callable from Ant)
     * <p>
     * Arguments (all required except -a):
     * <pre>
     * -f=[function name]   requested function name
     *                      Supported:
     *                        resolve-deps-dfs
     * -a=[function args]   function argument, may occur as many times as needed
     * -do=[output delim]   output list delimiter (such as ":")
     * -de=[elem delim]     element delimiter (such as "=") NOTE: "=" is allowed
     * -dd=[dep delim]      dependency delimiter (such as "," or ":")
     * -g=[graph entry]     graph entry (complex), may occur as many times as needed;
     *                      each entry is in the format:
     *                        element=dependency1:dependency2,...
     *                      where "=" is the element delimiter and ":" is the dependency delimiter
     * </pre>
     * <p>
     * TODO: better argument parsing
     */
    public static void main(String[] argsArr) {
        CmdArgs args = new CmdArgs(argsArr, Arrays.asList("f", "a", "do", "de", "dd", "g"));
        
        final String[] supportedFuncs = new String[] { "resolve-deps-dfs" };
        String func = args.getSingleRequiredNonEmptyArgValue("f");
        if ("resolve-deps-dfs".equals(func)) {
            String outDelim = args.getSingleRequiredNonEmptyArgValue("do");
            String elemDelim = args.getSingleRequiredNonEmptyArgValue("de");
            String depDelim = args.getSingleRequiredNonEmptyArgValue("dd");
            
            List<String> strDepGraph = args.getRequiredArgValues("g");
            DependencyGraph<String> depGraph = fromStringDependencyList(strDepGraph, elemDelim, depDelim);
            List<String> resolved = depGraph.getResolvedDependenciesDfs();
                    
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
            this.args = toArgsMap(args);
            this.allowedArgNames = (allowedArgNames instanceof Set) ? (Set<String>) allowedArgNames : new HashSet<String>(allowedArgNames);
        }
        
        public CmdArgs(String[] args, Collection<String> allowedArgNames) {
            this(Arrays.asList(args), allowedArgNames);
        }

        private Map<String, List<String>> toArgsMap(List<String> args) {
            Map<String, List<String>> argsMap = new LinkedHashMap<>();
            for(String arg : args) {
                if (arg.startsWith("-")) {
                    String[] parts = arg.split("=", 2);
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
                throw new IllegalArgumentException("Unrecognized parameter: -" + name);
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
            if (vals == null || vals.isEmpty()) {
                throw new IllegalArgumentException("Missing required argument: " + name);
            }
            return vals;
        }
        
        public String getFirstArgValue(String name) {
            List<String> vals = getArgValues(name);
            if (vals != null && !vals.isEmpty()) {
                return vals.get(0);
            }
            return null;
        }
        
        public String getSingleRequiredArgValue(String name) {
            List<String> vals = getArgValues(name);
            if (vals == null || vals.isEmpty()) {
                throw new IllegalArgumentException("Missing required argument: " + name);
            }
            if (vals.size() != 1) {
                throw new IllegalArgumentException("Too many instances of argument passed: " + name);
            }
            return vals.get(0);
        }
        
        public String getSingleRequiredNonEmptyArgValue(String name) {
            String val = getSingleRequiredArgValue(name);
            if (val == null || val.isEmpty()) {
                throw new IllegalArgumentException("Missing required argument value: " + name);
            }
            return val;
        }
        
    }
    
    private static String join(Collection<String> coll, String delim) { // (to avoid dependencies)
        StringBuilder sb = new StringBuilder();
        for(String str : coll) {
            sb.append(delim);
            sb.append(str);
        }
        return sb.substring(delim.length());
    }
    
}
