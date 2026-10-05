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
package org.ofbiz.widget.model;

import java.io.Serializable;

/**
 * SCIPIO: extra helper class to parse locations.
 */
@SuppressWarnings("serial")
public class ModelLocation implements Serializable {

    public static final ModelLocation EMPTY_LOCATION = new ModelLocation("", "", false);

    protected final String name;
    protected final String resource;

    protected ModelLocation(String resource, String name, boolean resourceAsAddress) {
        if (resourceAsAddress) {
            // Here, resource doubles as a full address
            String[] resourceParts = resource.split("#", 2);

            if (name != null && !name.isEmpty()) {
                // explicit name always overrides resource name
                this.name = name;
                this.resource = resourceParts[0];
            } else {
                if (resourceParts.length >= 2) {
                    this.name = resourceParts[1];
                    this.resource = resourceParts[0];
                } else {
                    // SCIPIO: 4.0.0: Handle class:// URLs with nested class syntax ($)
                    // For class://pkg.OuterClass$NestedClass, parse as:
                    // - resource = full class:// URL (for MenuFactory lookup)
                    // - name = NestedClass (the menu name for annotation cache)
                    if (resourceParts[0].startsWith("class://")) {
                        String classPath = resourceParts[0];
                        int dollarIndex = classPath.lastIndexOf('$');
                        if (dollarIndex > 0) {
                            this.resource = classPath;
                            this.name = classPath.substring(dollarIndex + 1);
                        } else {
                            // No $, use simple class name as menu name
                            this.resource = classPath;
                            int lastDot = classPath.lastIndexOf('.');
                            this.name = lastDot > 0 ? classPath.substring(lastDot + 1) : classPath.substring("class://".length());
                        }
                    } else {
                        // Original behavior for non-class:// URLs
                        this.name = resourceParts[0];
                        this.resource = "";
                    }
                }
            }

        } else {
            if (name == null) {
                this.name = "";
            } else {
                this.name = name;
            }
            if (resource == null) {
                this.resource = "";
            } else {
                this.resource = resource;
            }
        }
    }

    protected ModelLocation(String resource, String name) {
        this(resource, name, false);
    }


    protected ModelLocation(String address) {
        this(address, "", true);
    }

    @Override
    public String toString() {
        return getCombinedName();
    }

    public String getCombinedName() {
        return resource + "#" + name;
    }

    public String getName() {
        return name;
    }

    public boolean hasName() {
        return !name.isEmpty();
    }

    public String getResource() {
        return resource;
    }

    public boolean hasResource() {
        return !resource.isEmpty();
    }

    public boolean isEmpty() {
        return resource.isEmpty() && name.isEmpty();
    }

    public String getResource(String defaultResource) {
        return resource.isEmpty() ? ((defaultResource == null) ? "" : defaultResource) : resource;
    }

    public ModelLocation withDefaultResource(String defaultResource) {
        return new ModelLocation(getResource(defaultResource), getName(), false);
    }

    /**
     * Splits location from "resource#name" string.
     */
    public static ModelLocation fromAddress(String address) {
        return new ModelLocation(address);
    }

    public static ModelLocation fromAddress(String address, String defaultResource) {
        return new ModelLocation(address).withDefaultResource(defaultResource);
    }

    public static ModelLocation fromResAndName(String resource, String name) {
        return new ModelLocation(resource, name, false);
    }

    public static ModelLocation fromResAndName(String resource, String name, String defaultResource) {
        return new ModelLocation(resource, name, false).withDefaultResource(defaultResource);
    }

    /**
     * Here, resource can double as a full address ("resource#name"), though if name is specified it will
     * always be used.
     */
    public static ModelLocation fromResAndNameOrAddress(String resource, String name) {
        return new ModelLocation(resource, name, true);
    }

    public static ModelLocation fromResAndNameOrAddress(String resource, String name, String defaultResource) {
        return new ModelLocation(resource, name, true).withDefaultResource(defaultResource);
    }

}
