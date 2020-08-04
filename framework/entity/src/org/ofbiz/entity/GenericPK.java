/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.entity;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;

import org.ofbiz.entity.model.ModelEntity;

/**
 * Generic Entity Primary Key Object
 *
 */
@SuppressWarnings("serial")
public class GenericPK extends GenericEntity {

    protected GenericPK() { }

    /** Creates new GenericPK */
    public static GenericPK create(ModelEntity modelEntity) {
        GenericPK newPK = new GenericPK();
        newPK.init(modelEntity);
        return newPK;
    }

    /** Creates new GenericPK from existing Map */
    public static GenericPK create(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fields, Object fieldNames) { // SCIPIO: fieldNames
        GenericPK newPK = new GenericPK();
        newPK.init(delegator, modelEntity, fields, fieldNames);
        return newPK;
    }

    /** Creates new GenericPK from existing Map */
    public static GenericPK create(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fields) {
        GenericPK newPK = new GenericPK();
        newPK.init(delegator, modelEntity, fields);
        return newPK;
    }

    /** Creates new GenericPK from existing Map */
    public static GenericPK create(Delegator delegator, ModelEntity modelEntity, Object singlePkValue) {
        GenericPK newPK = new GenericPK();
        newPK.init(delegator, modelEntity, singlePkValue);
        return newPK;
    }

    /** Creates new GenericPK from existing GenericPK */
    public static GenericPK create(GenericPK value) {
        GenericPK newPK = new GenericPK();
        newPK.init(value);
        return newPK;
    }

    /** SCIPIO: Creates new GenericValue partially from fields from existing GenericValue with new-to-existing field name mappings, but treated as a "new" instance (not a "copy");
     * source fields are assumed to already be correct/same types as those on the new value (no type checks).<p>
     * NOTE: Instance members other than "fields" are treated as a "new" value, not copied from the passed value; this is half-way between
     * copy constructor and construction from map. Added 2018-10-22. */
    public static GenericPK createAsFieldSubset(Delegator delegator, ModelEntity modelEntity, Map<String, Object> sourceFieldsValue, Object fieldNames) {
        GenericPK newValue = new GenericPK();
        newValue.initAsFieldSubset(delegator, modelEntity, sourceFieldsValue, fieldNames);
        return newValue;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(super.hashCode());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof GenericPK) {
            return super.equals(obj);
        }
        return false;
    }

    /** Clones this GenericPK, this is a shallow clone and uses the default shallow HashMap clone
     *  @return Object that is a clone of this GenericPK
     */
    @Override
    public GenericPK clone() {
        return GenericPK.create(this);
    }

    @Override
    protected GenericPK newValue() { // SCIPIO
        return new GenericPK();
    }
}
