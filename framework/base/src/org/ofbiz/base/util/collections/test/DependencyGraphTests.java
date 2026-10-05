/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.base.util.collections.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.test.GenericTestCaseBase;

import com.ilscipio.scipio.ce.build.util.DependencyGraph;

/**
 * SCIPIO: DependencyGraph tests.
 * TODO: Proper test cases
 */
public class DependencyGraphTests extends GenericTestCaseBase {

    public DependencyGraphTests(String name) {
        super(name);
    }

    /**
     * TODO: Proper test case instead of debug lines
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
            System.out.println("Scipio: Dependency graph test 1:"
                    + "\nOriginal order: " + new ArrayList<String>(in.keySet()).toString()
                    + "\nResolved order: " + resolved.toString());

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
            System.out.println("Scipio: Dependency graph test 2: order should be unchanged: "
                    + "\nOriginal order: " + new ArrayList<String>(in.keySet()).toString()
                    + "\nResolved order: " + resolved.toString());

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
                System.err.println("ERROR: Scipio: Dependency graph test 3: ERROR: failed to detect circular dependency: "
                    + "\nOriginal order: " + new ArrayList<String>(in.keySet()).toString()
                    + "\nResolved order: " + resolved.toString());
            } catch(IllegalStateException e) {
                System.out.println("Scipio: Dependency graph test 3: the following error message should be a circular"
                        + " dependency error: " + e.getMessage());
            }
        } catch (Exception e) {
            System.err.println("ERROR: Scipio: Unexpected dependency graph test error: " + e.getMessage());
        }
    }

}
