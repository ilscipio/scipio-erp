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
                    this.name = resourceParts[0];
                    this.resource = "";
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
