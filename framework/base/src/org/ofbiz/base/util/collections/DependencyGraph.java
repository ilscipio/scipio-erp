package org.ofbiz.base.util.collections;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * SCIPIO: Represents a graph of dependencies, support algorithm(s) to resolve them.
 * <p>
 * This saves an initial list of elements and performs the algorithm on the indexes of the elements
 * in the list, faster than hashing strings constantly.
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
}
