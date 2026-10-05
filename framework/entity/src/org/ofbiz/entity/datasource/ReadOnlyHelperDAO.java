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
package org.ofbiz.entity.datasource;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericPK;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelRelation;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;

/**
 * Read Only Entity Helper Class
 *
 */
public class ReadOnlyHelperDAO implements GenericHelper {

    public static final String module = ReadOnlyHelperDAO.class.getName();

    protected GenericDAO genericDAO;
    protected GenericHelperInfo helperInfo;

    public ReadOnlyHelperDAO(GenericHelperInfo helperInfo) {
        this.helperInfo = helperInfo;
        genericDAO = GenericDAO.getGenericDAO(helperInfo);
    }

    public String getHelperName() {
        return this.helperInfo.getHelperFullName();
    }

    /** Read only, no creation realize on the database
     *@return null
     */
    public GenericValue create(GenericValue value) throws GenericEntityException {
        return null;
    }

    /** Find a Generic Entity by its Primary Key
     *@param primaryKey The primary key to find by.
     *@return The GenericValue corresponding to the primaryKey
     */
    public GenericValue findByPrimaryKey(GenericPK primaryKey) throws GenericEntityException {
        if (primaryKey == null) {
            return null;
        }
        GenericValue genericValue = GenericValue.create(primaryKey);

        genericDAO.select(genericValue);
        return genericValue;
    }

    /** Find a Generic Entity by its Primary Key and only returns the values requested by the passed keys (names)
     *@param primaryKey The primary key to find by.
     *@param keys The keys, or names, of the values to retrieve; only these values will be retrieved
     *@return The GenericValue corresponding to the primaryKey
     */
    public GenericValue findByPrimaryKeyPartial(GenericPK primaryKey, Set<String> keys) throws GenericEntityException {
        if (primaryKey == null) {
            return null;
        }
        GenericValue genericValue = GenericValue.create(primaryKey);

        genericDAO.partialSelect(genericValue, keys);
        return genericValue;
    }

    /** Find a number of Generic Value objects by their Primary Keys, all at once
     * This is done here for the DAO GenericHelper; for a client-server helper it
     * would be done on the server side to reduce network round trips.
     *@param primaryKeys A List of primary keys to find by.
     *@return List of GenericValue objects corresponding to the passed primaryKey objects
     */
    public List<GenericValue> findAllByPrimaryKeys(List<GenericPK> primaryKeys) throws GenericEntityException {
        if (primaryKeys == null) return null;
        List<GenericValue> results = new LinkedList<GenericValue>();

        for (GenericPK primaryKey: primaryKeys) {
            GenericValue result = this.findByPrimaryKey(primaryKey);

            if (result != null) results.add(result);
        }
        return results;
    }

    /** Read only, no remove realize on the database
     *@return 0
     */
    public int removeByPrimaryKey(GenericPK primaryKey) throws GenericEntityException {
        return 0;
    }

    /** Finds GenericValues by the conditions specified in the EntityCondition object, the the EntityCondition javadoc for more details.
     *@param modelEntity The ModelEntity of the Entity as defined in the entity XML file
     *@param whereEntityCondition The EntityCondition object that specifies how to constrain this query before any groupings are done (if this is a view entity with group-by aliases)
     *@param havingEntityCondition The EntityCondition object that specifies how to constrain this query after any groupings are done (if this is a view entity with group-by aliases)
     *@param fieldsToSelect The fields of the named entity to get from the database; if empty or null all fields will be retreived
     *@param orderBy The fields of the named entity to order the query by; optionally add a " ASC" for ascending or " DESC" for descending
     *@param findOptions An instance of EntityFindOptions that specifies advanced query options. See the EntityFindOptions JavaDoc for more details.
     *@return EntityListIterator representing the result of the query: NOTE THAT THIS MUST BE CLOSED WHEN YOU ARE
     *      DONE WITH IT, AND DON'T LEAVE IT OPEN TOO LONG BEACUSE IT WILL MAINTAIN A DATABASE CONNECTION.
     */
    public EntityListIterator findListIteratorByCondition(Delegator delegator, ModelEntity modelEntity, EntityCondition whereEntityCondition,
        EntityCondition havingEntityCondition, Collection<String> fieldsToSelect, List<String> orderBy, EntityFindOptions findOptions)
        throws GenericEntityException {
        return genericDAO.selectListIteratorByCondition(delegator, modelEntity, whereEntityCondition, havingEntityCondition, fieldsToSelect, orderBy, findOptions);
    }

    public List<GenericValue> findByMultiRelation(GenericValue value, ModelRelation modelRelationOne, ModelEntity modelEntityOne,
        ModelRelation modelRelationTwo, ModelEntity modelEntityTwo, List<String> orderBy) throws GenericEntityException {
        return genericDAO.selectByMultiRelation(value, modelRelationOne, modelEntityOne, modelRelationTwo, modelEntityTwo, orderBy);
    }

    public long findCountByCondition(Delegator delegator, ModelEntity modelEntity, EntityCondition whereEntityCondition, EntityCondition havingEntityCondition, EntityFindOptions findOptions) throws GenericEntityException {
        return genericDAO.selectCountByCondition(delegator, modelEntity, whereEntityCondition, havingEntityCondition, findOptions);
    }


    /** Read only, no remove realize on the database
     *@return 0
     */
    public int removeByCondition(Delegator delegator, ModelEntity modelEntity, EntityCondition condition) throws GenericEntityException {
        return 0;
    }

    /** Read only, no store realize on the database
     *@return 0
     */
    public int store(GenericValue value) throws GenericEntityException {
        return 0;
    }

    /** Read only, no store realize on the database
     *@return 0
     */
    public int storeByCondition(Delegator delegator, ModelEntity modelEntity, Map<String, ? extends Object> fieldsToSet, EntityCondition condition) throws GenericEntityException {
        return 0;
    }

    /** Check the datasource to make sure the entity definitions are correct, optionally adding missing entities or fields on the server
     *@param modelEntities Map of entityName names and ModelEntity values
     *@param messages List to put any result messages in
     *@param addMissing Flag indicating whether or not to add missing entities and fields on the server by force to false on read only mode
     */
    public void checkDataSource(Map<String, ModelEntity> modelEntities, List<String> messages, boolean addMissing) throws GenericEntityException {
        genericDAO.checkDb(modelEntities, messages, false);
    }
}
