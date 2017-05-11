package org.ofbiz.widget.model.ftl;

/**
 * Represents <code>@virtualSection</code> implementation - meant to be the FTL
 * equivalent of widget <code>section</code> element 
 * (because <code>@section</code> already matches <code>screenlet</code>).
 */
@SuppressWarnings("serial")
public class ModelVirtualSectionFtlWidget extends ModelFtlWidget {
    public ModelVirtualSectionFtlWidget(String name, String location) {
        super(name, "virtualSection", location, null);
    }
}