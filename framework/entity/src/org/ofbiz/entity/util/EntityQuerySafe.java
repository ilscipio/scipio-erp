package org.ofbiz.entity.util;

import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.collections.PagedList;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.model.DynamicViewEntity;

/**
 * SCIPIO: A version of EntityQuery that avoids throwing GenericEntityException,
 * though it <em>may</em> still throw programming-related unchecked exceptions.
 */
public class EntityQuerySafe extends EntityQuery {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public EntityQuerySafe(Delegator delegator) {
        super(delegator);
    }

    @Override
    public EntityQuerySafe select(Set<String> fieldsToSelect) {
        return (EntityQuerySafe) super.select(fieldsToSelect);
    }

    @Override
    public EntityQuerySafe select(String... fields) {
        return (EntityQuerySafe) super.select(fields);
    }

    @Override
    public EntityQuerySafe from(String entityName) {
        return (EntityQuerySafe) super.from(entityName);
    }

    @Override
    public EntityQuerySafe from(DynamicViewEntity dynamicViewEntity) {
        return (EntityQuerySafe) super.from(dynamicViewEntity);
    }

    @Override
    public EntityQuerySafe where(EntityCondition entityCondition) {
        return (EntityQuerySafe) super.where(entityCondition);
    }

    @Override
    public EntityQuerySafe where(Map<String, Object> fieldMap) {
        return (EntityQuerySafe) super.where(fieldMap);
    }

    @Override
    public EntityQuerySafe where(Object... fields) {
        return (EntityQuerySafe) super.where(fields);
    }

    @Override
    public EntityQuerySafe where(EntityCondition... entityCondition) {
        return (EntityQuerySafe) super.where(entityCondition);
    }

    @Override
    public <T extends EntityCondition> EntityQuerySafe where(List<T> andConditions) {
        return (EntityQuerySafe) super.where(andConditions);
    }

    @Override
    public <T extends EntityCondition> EntityQuerySafe where(List<T> conditions, EntityJoinOperator operator) {
        return (EntityQuerySafe) super.where(conditions, operator);
    }

    @Override
    public EntityQuerySafe having(EntityCondition entityCondition) {
        return (EntityQuerySafe) super.having(entityCondition);
    }

    @Override
    public EntityQuerySafe orderBy(List<String> orderBy) {
        return (EntityQuerySafe) super.orderBy(orderBy);
    }

    @Override
    public EntityQuerySafe orderBy(String... fields) {
        return (EntityQuerySafe) super.orderBy(fields);
    }

    @Override
    public EntityQuerySafe cursorForwardOnly() {
        return (EntityQuerySafe) super.cursorForwardOnly();
    }

    @Override
    public EntityQuerySafe cursorScrollSensitive() {
        return (EntityQuerySafe) super.cursorScrollSensitive();
    }

    @Override
    public EntityQuerySafe cursorScrollInsensitive() {
        return (EntityQuerySafe) super.cursorScrollInsensitive();
    }

    @Override
    public EntityQuerySafe fetchSize(int fetchSize) {
        return (EntityQuerySafe) super.fetchSize(fetchSize);
    }

    @Override
    public EntityQuerySafe maxRows(int maxRows) {
        return (EntityQuerySafe) super.maxRows(maxRows);
    }

    @Override
    public EntityQuerySafe distinct() {
        return (EntityQuerySafe) super.distinct();
    }

    @Override
    public EntityQuerySafe distinct(boolean distinct) {
        return (EntityQuerySafe) super.distinct(distinct);
    }

    @Override
    public EntityQuerySafe cache() {
        return (EntityQuerySafe) super.cache();
    }

    @Override
    public EntityQuerySafe cache(boolean useCache) {
        return (EntityQuerySafe) super.cache(useCache);
    }

    @Override
    public EntityQuerySafe filterByDate() {
        return (EntityQuerySafe) super.filterByDate();
    }

    @Override
    public EntityQuerySafe filterByDate(Timestamp moment) {
        return (EntityQuerySafe) super.filterByDate(moment);
    }

    @Override
    public EntityQuerySafe filterByDate(Date moment) {
        return (EntityQuerySafe) super.filterByDate(moment);
    }

    @Override
    public EntityQuerySafe filterByDate(String... filterByFieldName) {
        return (EntityQuerySafe) super.filterByDate(filterByFieldName);
    }

    @Override
    public EntityQuerySafe filterByDate(Timestamp moment, String... filterByFieldName) {
        return (EntityQuerySafe) super.filterByDate(moment, filterByFieldName);
    }

    @Override
    public EntityQuerySafe filterByDate(boolean enable) {
        return (EntityQuerySafe) super.filterByDate(enable);
    }

    @Override
    public EntityQuerySafe filterByDate(boolean enable, Timestamp moment) {
        return (EntityQuerySafe) super.filterByDate(enable, moment);
    }

    @Override
    public EntityQuerySafe filterByDate(boolean enable, Date moment) {
        return (EntityQuerySafe) super.filterByDate(enable, moment);
    }

    @Override
    public EntityQuerySafe filterByDate(boolean enable, String... filterByFieldName) {
        return (EntityQuerySafe) super.filterByDate(enable, filterByFieldName);
    }

    @Override
    public EntityQuerySafe filterByDate(boolean enable, Timestamp moment, String... filterByFieldName) {
        return (EntityQuerySafe) super.filterByDate(enable, moment, filterByFieldName);
    }

    @Override
    public List<GenericValue> queryList() {
        try {
            return super.queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryList(): " + e.getMessage(), module);
            return null;
        }
    }

    @Override
    public EntityListIterator queryIterator() {
        try {
            return super.queryIterator();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryIterator(): " + e.getMessage(), module);
            return null;
        }
    }

    @Override
    public GenericValue queryFirst() {
        try {
            return super.queryFirst();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryFirst(): " + e.getMessage(), module);
            return null;
        }
    }

    @Override
    public GenericValue queryOne() {
        try {
            return super.queryOne();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryOne(): " + e.getMessage(), module);
            return null;
        }
    }

    @Override
    public long queryCount() {
        try {
            return super.queryCount();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryCount(): " + e.getMessage(), module);
            return 0;
        }
    }

    @Override
    public <T> List<T> getFieldList(String fieldName) {
        try {
            return super.getFieldList(fieldName);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in getFieldList(): " + e.getMessage(), module);
            return null;
        }
    }

    @Override
    public PagedList<GenericValue> queryPagedList(int viewIndex, int viewSize) {
        try {
            return super.queryPagedList(viewIndex, viewSize);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in queryPagedList(): " + e.getMessage(), module);
            return null;
        }
    }
}
