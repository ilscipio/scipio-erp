package org.ofbiz.base.util.collections;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: Represents a graph of dependencies, support algorithm(s) to resolve them.
 * <p>
 * This saves an initial list of elements and performs the algorithm on the indexes of the elements
 * in the list, faster than hashing strings constantly.
 */
public class DependencyGraph<T> {

    public static final String module = DependencyGraph.class.getName();
    
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
        int[][] edges = new int[elements.size()][];
        int i=0;
        for(T element : elements) {
            List<T> deps = dependencies.get(element);
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
     * TODO: Proper test case
     */
    public static void runGraphTest() {
        try {
            Map<String, List<String>> in;
            DependencyGraph<String> depGraph;
            List<String> resolved;
            
            in = new java.util.LinkedHashMap<>();
            in.put("testcomp1", Arrays.asList("testcomp8"));
            in.put("testcomp2", Arrays.asList("testcomp1"));
            in.put("testcomp3", Arrays.asList("testcomp6", "testcomp2"));
            in.put("testcomp4", Arrays.asList("testcomp3", "testcomp2"));
            in.put("testcomp5", Arrays.asList("testcomp4"));
            in.put("testcomp6", new ArrayList<String>());
            in.put("testcomp7", Arrays.asList("testcomp6", "testcomp8"));
            in.put("testcomp8", Arrays.asList("testcomp6"));
            depGraph = new DependencyGraph<>(in);
            resolved = depGraph.getResolvedDependenciesDfs();
            Debug.logInfo("SCIPIO: Dependency graph test 1:"
                    + "\nOriginal order: " + new ArrayList<String>(in.keySet()).toString()
                    + "\nResolved order: " + resolved.toString(), module);
            
            in = new java.util.LinkedHashMap<>();
            in.put("testcomp1", new ArrayList<String>());
            in.put("testcomp2", Arrays.asList("testcomp1"));
            in.put("testcomp3", Arrays.asList("testcomp2"));
            in.put("testcomp4", Arrays.asList("testcomp3", "testcomp2"));
            in.put("testcomp5", Arrays.asList("testcomp4"));
            in.put("testcomp6", new ArrayList<String>());
            in.put("testcomp7", Arrays.asList("testcomp6", "testcomp1"));
            in.put("testcomp8", Arrays.asList("testcomp6", "testcomp4"));
            depGraph = new DependencyGraph<>(in);
            resolved = depGraph.getResolvedDependenciesDfs();
            Debug.logInfo("SCIPIO: Dependency graph test 2: order should be unchanged: "
                    + "\nOriginal order: " + new ArrayList<String>(in.keySet()).toString()
                    + "\nResolved order: " + resolved.toString(), module);
            
            in = new java.util.LinkedHashMap<>();
            in.put("testcomp1", Arrays.asList("testcomp8"));
            in.put("testcomp2", Arrays.asList("testcomp1"));
            in.put("testcomp3", Arrays.asList("testcomp6", "testcomp2"));
            in.put("testcomp4", Arrays.asList("testcomp3", "testcomp2"));
            in.put("testcomp5", Arrays.asList("testcomp4"));
            in.put("testcomp6", Arrays.asList("testcomp1"));
            in.put("testcomp7", Arrays.asList("testcomp6", "testcomp8"));
            in.put("testcomp8", Arrays.asList("testcomp6"));
            depGraph = new DependencyGraph<>(in);
            try {
                resolved = depGraph.getResolvedDependenciesDfs();
                Debug.logError("SCIPIO: Dependency graph test 3: ERROR: failed to detect circular dependency: "
                    + "\nOriginal order: " + new ArrayList<String>(in.keySet()).toString()
                    + "\nResolved order: " + resolved.toString(), module);
            } catch(IllegalStateException e) {
                Debug.logInfo("SCIPIO: Dependency graph test 3: the following error message should be a circular"
                        + " dependency error: " + e.getMessage(), module);
            }
        } catch (Exception e) {
            Debug.logError(e, "SCIPIO: Unexpected dependency graph test error: " + e.getMessage(), module);
        }
    }

}
