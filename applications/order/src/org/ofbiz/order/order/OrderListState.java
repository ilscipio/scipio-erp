/*
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
 */
package org.ofbiz.order.order;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilDateTime.TimeInterval;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityQuery;

/**
 * Session object for keeping track of the list of orders.
 * The state of the list is preserved here instead of
 * via url parameters, which can get messy.  There
 * are three types of state:  Order State, Order Type,
 * and pagination position.
 *
 * Also provides convenience methods for retrieving
 * the right set of data for a particular state.
 *
 * TODO: this can be generalized to use a set of State
 * objects, including Pagination. Think about design
 * patterns in Fowler.
 * <p>
 * SCIPIO: 2018-11-28: Entire class is modified to be read-only. 
 * The {@link #update(HttpServletRequest)} method now returns a new instance.
 * This is to ensure some measure of thread-safety and atomic updates and reads,
 * though some updates may still be lost due to not synchronizing on the session read/write.
 * Also {@link #getOrders} result has been modified.
 * FIXME?: {@link #getInstance(HttpServletRequest)} and {@link #update(HttpServletRequest)} should lock
 * on a dedicated session lock to prevent lost updates, but currently this makes practically no difference.
 */
@SuppressWarnings("serial")
public class OrderListState implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String SESSION_KEY = "__ORDER_LIST_STATUS__";
    public static final String VIEW_SIZE_PARAM = "viewSize";
    public static final String VIEW_INDEX_PARAM = "viewIndex";

    // SCIPIO: 2018-11-28: All fields now final.
    
    // state variables
    protected final int viewSize;
    protected final int viewIndex;
    protected final Map<String, String> orderStatusState;
    protected final Map<String, String> orderTypeState;
    protected final Map<String, String> orderFilterState;
    //protected int orderListSize; // SCIPIO: REMOVED and now returned by getOrders method

    // parameter to ID maps
    protected static final Map<String, String> parameterToOrderStatusId;
    protected static final Map<String, String> parameterToOrderTypeId;
    protected static final Map<String, String> parameterToFilterId;
    static {
        Map<String, String> map = new HashMap<>();
        map.put("viewcompleted", "ORDER_COMPLETED");
        map.put("viewcancelled", "ORDER_CANCELLED");
        map.put("viewrejected", "ORDER_REJECTED");
        map.put("viewapproved", "ORDER_APPROVED");
        map.put("viewcreated", "ORDER_CREATED");
        map.put("viewprocessing", "ORDER_PROCESSING");
        map.put("viewhold", "ORDER_HOLD");
        parameterToOrderStatusId = map;

        map = new HashMap<>();
        map.put("view_SALES_ORDER", "SALES_ORDER");
        map.put("view_PURCHASE_ORDER", "PURCHASE_ORDER");
        parameterToOrderTypeId = map;

        map = new HashMap<>();
        map.put("filterInventoryProblems", "filterInventoryProblems");
        map.put("filterAuthProblems", "filterAuthProblems");
        map.put("filterPartiallyReceivedPOs", "filterPartiallyReceivedPOs");
        map.put("filterPOsOpenPastTheirETA", "filterPOsOpenPastTheirETA");
        map.put("filterPOsWithRejectedItems", "filterPOsWithRejectedItems");
        parameterToFilterId = map;
    }

    //=============   Initialization and Request methods   ===================//

    /**
     * Initializes the order list state with default values. Do not use directly,
     * instead use getInstance().
     */
    protected OrderListState() {
        // SCIPIO: unhardcode default
        // viewSize = 10;
        viewSize = UtilProperties.getPropertyAsInteger("order.properties", "order.paginate.defaultViewSize", 10);
        viewIndex = 0;
        // SCIPIO: Local vars
        Map<String, String> orderStatusState = new HashMap<>();
        Map<String, String> orderTypeState = new HashMap<>();
        Map<String, String> orderFilterState = new HashMap<>();

        // defaults (TODO: configuration)
        orderStatusState.put("viewcreated", "Y");
        orderStatusState.put("viewprocessing", "Y");
        orderStatusState.put("viewapproved", "Y");
        orderStatusState.put("viewhold", "N");
        orderStatusState.put("viewcompleted", "N");
        orderStatusState.put("viewsent", "N");
        orderStatusState.put("viewrejected", "N");
        orderStatusState.put("viewcancelled", "N");
        orderTypeState.put("view_SALES_ORDER", "Y");
        
        this.orderStatusState = Collections.unmodifiableMap(orderStatusState);
        this.orderTypeState = Collections.unmodifiableMap(orderTypeState);
        this.orderFilterState = Collections.unmodifiableMap(orderFilterState);

        //orderListSize = 0; // SCIPIO
    }

    protected OrderListState(OrderListState other, HttpServletRequest request) { // SCIPIO: copy/update constructor
        this.viewSize = other.viewSize;
        this.viewIndex = 0; // SCIPIO: see changeOrderListStates
        
        Map<String, String> orderStatusState = new HashMap<>(other.orderStatusState);
        Map<String, String> orderTypeState = new HashMap<>(other.orderTypeState);
        Map<String, String> orderFilterState = new HashMap<>(other.orderFilterState);

        changeOrderListStates(request, orderStatusState, orderTypeState, orderFilterState);
        
        this.orderStatusState = Collections.unmodifiableMap(orderStatusState);
        this.orderTypeState = Collections.unmodifiableMap(orderTypeState);
        this.orderFilterState = Collections.unmodifiableMap(orderFilterState);

        //this.orderListSize = other.orderListSize;
    }

    protected OrderListState(OrderListState other, int viewSize, int viewIndex) { // SCIPIO: view copy/update constructor
        this.viewSize = viewSize;
        this.viewIndex = viewIndex;
        this.orderStatusState = other.orderStatusState;
        this.orderTypeState = other.orderTypeState;
        this.orderFilterState = other.orderFilterState;
        //this.orderListSize = other.orderListSize;
    }
    
    /**
     * Retrieves the current user's OrderListState from the session
     * or creates a new one with defaults.
     */
    public static OrderListState getInstance(HttpServletRequest request) {
        HttpSession session = request.getSession();
        OrderListState status = (OrderListState) session.getAttribute(SESSION_KEY);
        if (status == null) {
            status = new OrderListState();
            session.setAttribute(SESSION_KEY, status);
        }
        return status;
    }
    
    /**
     * Given a request, decides what state to change.  If a parameter changeStatusAndTypeState
     * is present with value "Y", the status and type state will be updated.  Otherwise, if the
     * viewIndex and viewSize parameters are present, the pagination changes.
     */
    public OrderListState update(HttpServletRequest request) { // SCIPIO: Now returns a new instance
        OrderListState status = null;
        if ("Y".equals(request.getParameter("changeStatusAndTypeState"))) {
            // SCIPIO
            //changeOrderListStates(request);
            status = new OrderListState(this, request);
        } else {
            String viewSizeParam = request.getParameter(VIEW_SIZE_PARAM);
            String viewIndexParam = request.getParameter(VIEW_INDEX_PARAM);
            if (UtilValidate.isNotEmpty(viewSizeParam) && UtilValidate.isNotEmpty(viewIndexParam)) {
                // SCIPIO
                //changePaginationState(viewSizeParam, viewIndexParam);
                try {
                    int viewSize = Integer.parseInt(viewSizeParam);
                    int viewIndex = Integer.parseInt(viewIndexParam);
                    status = new OrderListState(this, viewSize, viewIndex);
                } catch (NumberFormatException e) {
                    Debug.logWarning("Values of " + VIEW_SIZE_PARAM + " ["+viewSizeParam+"] and " + VIEW_INDEX_PARAM + " ["+viewIndexParam+"] must both be Integers. Not paginating order list.", module);
                }
            }
        }
        if (status != null) {
            request.getSession().setAttribute(SESSION_KEY, status);
            return status;
        }
        return this;
    }

    /*
    private void changePaginationState(String viewSizeParam, String viewIndexParam) {
        try {
            viewSize = Integer.parseInt(viewSizeParam);
            viewIndex = Integer.parseInt(viewIndexParam);
        } catch (NumberFormatException e) {
            Debug.logWarning("Values of " + VIEW_SIZE_PARAM + " ["+viewSizeParam+"] and " + VIEW_INDEX_PARAM + " ["+viewIndexParam+"] must both be Integers. Not paginating order list.", module);
        }
    }
    */

    private static void changeOrderListStates(HttpServletRequest request,
            Map<String, String> orderStatusState, Map<String, String> orderTypeState, Map<String, String> orderFilterState) { // SCIPIO: now static and takes params
        for (String param : parameterToOrderStatusId.keySet()) {
            String value = request.getParameter(param);
            if ("Y".equals(value)) {
                orderStatusState.put(param, "Y");
            } else {
                orderStatusState.put(param, "N");
            }
        }
        for (String param : parameterToOrderTypeId.keySet()) {
            String value = request.getParameter(param);
            if ("Y".equals(value)) {
                orderTypeState.put(param, "Y");
            } else {
                orderTypeState.put(param, "N");
            }
        }
        for (String param : parameterToFilterId.keySet()) {
            String value = request.getParameter(param);
            if ("Y".equals(value)) {
                orderFilterState.put(param, "Y");
            } else {
                orderFilterState.put(param, "N");
            }
        }
        //viewIndex = 0; // SCIPIO: Handled by copy constructor
    }


    //==============   Get and Set methods   =================//


    public Map<String, String> getOrderStatusState() { return orderStatusState; }
    public Map<String, String> getOrderTypeState() { return orderTypeState; }
    public Map<String, String> getorderFilterState() { return orderFilterState; }

    /* SCIPIO: now immutable
    public void setStatus(String param, boolean b) { orderStatusState.put(param, (b ? "Y" : "N")); }
    public void setType(String param, boolean b) { orderTypeState.put(param, (b ? "Y" : "N")); }
    */

    public boolean hasStatus(String param) { return ("Y".equals(orderStatusState.get(param))); }
    public boolean hasType(String param) { return ("Y".equals(orderTypeState.get(param))); }
    public boolean hasFilter(String param) { return ("Y".equals(orderFilterState.get(param))); }

    public boolean hasAllStatus() {
        for (String string : orderStatusState.values()) {
            if (!"Y".equals(string)) {
                return false;
            }
        }
        return true;
    }

    public int getViewSize() { return viewSize; }
    public int getViewIndex() { return viewIndex; }
    /**
     * @deprecated SCIPIO: 2018-11-28: Always returns zero; use result from {@link #getOrders(String, Timestamp, String, Map)} instead.
     */
    @Deprecated
    public int getSize() { return 0; } //return orderListSize; }

    public boolean hasPrevious() { return (viewIndex > 0); }
    public boolean hasNext() { return (viewIndex < getSize() / viewSize); }

    /**
     * Get the OrderHeaders corresponding to the state.
     * <p>
     * SCIPIO: 2018-11-28: This now returns an object having a getTotalOrders method instead of modifying the current instance with orderListSize.
     */
    public OrdersResult getOrders(String facilityId, Timestamp fromDate, String intervalPeriod, Map<String, Object> context)
            throws GenericEntityException {
        Delegator delegator = (Delegator) context.get("delegator");
        TimeZone timeZone = (TimeZone) context.get("timeZone");
        Locale locale = (Locale) context.get("locale");
        List<EntityCondition> allConditions = new LinkedList<>();

        if (facilityId != null) {
            allConditions.add(EntityCondition.makeCondition("originFacilityId", EntityOperator.EQUALS, facilityId));
        }

        if (fromDate != null) {
            List<EntityCondition> andExprs = new LinkedList<>();
            if (intervalPeriod != null) {
                TimeInterval intervalDates = UtilDateTime.getPeriodInterval(intervalPeriod, fromDate, locale, timeZone);
                context.put("intervalDates", intervalDates);
                andExprs.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, intervalDates.getDateBegin()));
                andExprs.add(EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, intervalDates.getDateEnd()));
            } else {
                andExprs.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
                andExprs.add(EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, fromDate));
            }
            allConditions.add(EntityCondition.makeCondition(andExprs, EntityOperator.AND));
        }

        List<EntityCondition> statusConditions = new LinkedList<>();
        for (String status : orderStatusState.keySet()) {
            if (!hasStatus(status)) {
                continue;
            }
            statusConditions.add(EntityCondition.makeCondition("statusId", EntityOperator.EQUALS, parameterToOrderStatusId.get(status)));
        }
        List<EntityCondition> typeConditions = new LinkedList<>();
        for (String type : orderTypeState.keySet()) {
            if (!hasType(type)) {
                continue;
            }
            typeConditions.add(EntityCondition.makeCondition("orderTypeId", EntityOperator.EQUALS, parameterToOrderTypeId.get(type)));
        }

        EntityCondition statusConditionsList = EntityCondition.makeCondition(statusConditions, EntityOperator.OR);
        EntityCondition typeConditionsList = EntityCondition.makeCondition(typeConditions, EntityOperator.OR);
        if (statusConditions.size() > 0) {
            allConditions.add(statusConditionsList);
        }
        if (typeConditions.size() > 0) {
            allConditions.add(typeConditionsList);
        }

        EntityQuery eq = EntityQuery.use(delegator).from("OrderHeader")
                .where(allConditions)
                .orderBy("orderDate DESC")
                .maxRows(viewSize * (viewIndex + 1))
                .cursorScrollInsensitive();

        try (EntityListIterator iterator = eq.queryIterator()) {
            // get subset corresponding to pagination state
            List<GenericValue> orders = iterator.getPartialList(viewSize * viewIndex, viewSize);
            // SCIPIO: return instead
            //orderListSize = iterator.getResultsSizeAfterPartialList();
            //return orders;
            int orderListSize = iterator.getResultsSizeAfterPartialList();
            return new OrdersResult(orders, orderListSize);
        }
    }

    public static class OrdersResult implements Serializable, List<GenericValue> { // SCIPIO
        private final List<GenericValue> orders;
        private final int orderListSize;

        OrdersResult(List<GenericValue> orders, int orderListSize) {
            this.orders = orders;
            this.orderListSize = orderListSize;
        }

        public int getTotalOrders() {
            return orderListSize;
        }

        @Override
        public int size() {
            return orders.size();
        }
        @Override
        public boolean isEmpty() {
            return orders.isEmpty();
        }
        @Override
        public boolean contains(Object o) {
            return orders.contains(o);
        }
        @Override
        public Iterator<GenericValue> iterator() {
            return orders.iterator();
        }
        @Override
        public Object[] toArray() {
            return orders.toArray();
        }
        @Override
        public <T> T[] toArray(T[] a) {
            return orders.toArray(a);
        }
        @Override
        public boolean add(GenericValue e) {
            return orders.add(e);
        }
        @Override
        public boolean remove(Object o) {
            return orders.remove(o);
        }
        @Override
        public boolean containsAll(Collection<?> c) {
            return orders.containsAll(c);
        }
        @Override
        public boolean addAll(Collection<? extends GenericValue> c) {
            return orders.addAll(c);
        }
        @Override
        public boolean addAll(int index, Collection<? extends GenericValue> c) {
            return orders.addAll(index, c);
        }
        @Override
        public boolean removeAll(Collection<?> c) {
            return orders.removeAll(c);
        }
        @Override
        public boolean retainAll(Collection<?> c) {
            return orders.retainAll(c);
        }
        @Override
        public void clear() {
            orders.clear();
        }
        @Override
        public boolean equals(Object o) {
            return orders.equals(o);
        }
        @Override
        public int hashCode() {
            return orders.hashCode();
        }
        @Override
        public GenericValue get(int index) {
            return orders.get(index);
        }
        @Override
        public GenericValue set(int index, GenericValue element) {
            return orders.set(index, element);
        }
        @Override
        public void add(int index, GenericValue element) {
            orders.add(index, element);
        }
        @Override
        public GenericValue remove(int index) {
            return orders.remove(index);
        }
        @Override
        public int indexOf(Object o) {
            return orders.indexOf(o);
        }
        @Override
        public int lastIndexOf(Object o) {
            return orders.lastIndexOf(o);
        }
        @Override
        public ListIterator<GenericValue> listIterator() {
            return orders.listIterator();
        }
        @Override
        public ListIterator<GenericValue> listIterator(int index) {
            return orders.listIterator(index);
        }
        @Override
        public List<GenericValue> subList(int fromIndex, int toIndex) {
            return orders.subList(fromIndex, toIndex);
        }
    }
    
    @Override
    public String toString() {
        StringBuilder buff = new StringBuilder("OrderListState:\n\t");
        buff.append("viewIndex=").append(viewIndex).append(", viewSize=").append(viewSize).append("\n\t");
        buff.append(getOrderStatusState().toString()).append("\n\t");
        buff.append(getOrderTypeState().toString()).append("\n\t");
        buff.append(getorderFilterState().toString()).append("\n\t");
        return buff.toString();
    }
}
