package com.ilscipio.scipio.cms.data;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

/**
 * Factors out some read operations from CmsDataObject.
 */
public interface CmsEntityReadable {

    public GenericValue getEntity();

    public String getEntityName();
    
    public Delegator getDelegator();
}
