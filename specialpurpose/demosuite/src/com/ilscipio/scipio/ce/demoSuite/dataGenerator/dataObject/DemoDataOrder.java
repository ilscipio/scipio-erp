package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

public class DemoDataOrder implements AbstractDataObject {

    private String orderId;
    private String orderName;
    private String orderType;
    private Timestamp orderDate;
    private String channel;
    private BigDecimal grandTotal;
    private BigDecimal remainingSubTotal;
    private List<DemoDataOrderItem> orderItems = new ArrayList<DemoDataOrderItem>();
    private List<DemoDataOrderRole> orderRoles = new ArrayList<DemoDataOrderRole>();
    private List<DemoDataOrderStatus> orderStatuses = new ArrayList<DemoDataOrderStatus>();

    public List<DemoDataOrderItem> getOrderItems() {
        return orderItems;
    }

    public void setOrderItems(List<DemoDataOrderItem> orderItems) {
        this.orderItems = orderItems;
    }

    public void addOrderItem(DemoDataOrderItem orderItem) {
        orderItems.add(orderItem);
    }

    public String getOrderId() {
        return orderId;
    }

    public void setOrderId(String orderId) {
        this.orderId = orderId;
    }

    public String getOrderName() {
        return orderName;
    }

    public void setOrderName(String orderName) {
        this.orderName = orderName;
    }

    public String getOrderType() {
        return orderType;
    }

    public void setOrderType(String orderType) {
        this.orderType = orderType;
    }

    public List<DemoDataOrderRole> getOrderRoles() {
        return orderRoles;
    }

    public void setOrderRoles(List<DemoDataOrderRole> orderRoles) {
        this.orderRoles = orderRoles;
    }

    public void addOrderRole(DemoDataOrderRole orderRole) {
        orderRoles.add(orderRole);
    }

    public Timestamp getOrderDate() {
        return orderDate;
    }

    public void setOrderDate(Timestamp orderDate) {
        this.orderDate = orderDate;
    }

    public List<DemoDataOrderStatus> getOrderStatuses() {
        return orderStatuses;
    }

    public void setOrderStatus(List<DemoDataOrderStatus> orderStatuses) {
        this.orderStatuses = orderStatuses;
    }

    public void addOrderStatus(DemoDataOrderStatus orderStatus) {
        orderStatuses.add(orderStatus);
    }

    public String getChannel() {
        return channel;
    }

    public void setChannel(String channel) {
        this.channel = channel;
    }

    public BigDecimal getRemainingSubTotal() {
        return remainingSubTotal;
    }

    public void setRemainingSubTotal(BigDecimal remainingSubTotal) {
        this.remainingSubTotal = remainingSubTotal;
    }

    public BigDecimal getGrandTotal() {
        return grandTotal;
    }

    public void setGrandTotal(BigDecimal grandTotal) {
        this.grandTotal = grandTotal;
    }

    public class DemoDataOrderItem {
        private String orderItemId;
        private String orderItemType;
        private String productId;
        private String productCatalogId;
        private String isPromo;
        private BigDecimal quantity;
        private BigDecimal selectedAmount;
        private BigDecimal unitPrice;
        private BigDecimal unitListPrice;
        private BigDecimal itemCost;
        private String statusId;

        public String getOrderItemId() {
            return orderItemId;
        }

        public void setOrderItemId(String orderItemId) {
            this.orderItemId = orderItemId;
        }

        public String getOrderItemType() {
            return orderItemType;
        }

        public void setOrderItemType(String orderItemType) {
            this.orderItemType = orderItemType;
        }

        public String getProductId() {
            return productId;
        }

        public void setProductId(String productId) {
            this.productId = productId;
        }

        public String getProductCatalogId() {
            return productCatalogId;
        }

        public void setProductCatalogId(String productCatalogId) {
            this.productCatalogId = productCatalogId;
        }

        public String getIsPromo() {
            return isPromo;
        }

        public void setIsPromo(String isPromo) {
            this.isPromo = isPromo;
        }

        public BigDecimal getQuantity() {
            return quantity;
        }

        public void setQuantity(BigDecimal quantity) {
            this.quantity = quantity;
        }

        public BigDecimal getSelectedAmount() {
            return selectedAmount;
        }

        public void setSelectedAmount(BigDecimal selectedAmount) {
            this.selectedAmount = selectedAmount;
        }

        public String getStatusId() {
            return statusId;
        }

        public void setStatusId(String statusId) {
            this.statusId = statusId;
        }

        public BigDecimal getUnitPrice() {
            return unitPrice;
        }

        public void setUnitPrice(BigDecimal unitPrice) {
            this.unitPrice = unitPrice;
        }

        public BigDecimal getUnitListPrice() {
            return unitListPrice;
        }

        public void setUnitListPrice(BigDecimal unitListPrice) {
            this.unitListPrice = unitListPrice;
        }

        public BigDecimal getItemCost() {
            return itemCost;
        }

        public void setItemCost(BigDecimal itemCost) {
            this.itemCost = itemCost;
        }
    }

    public class DemoDataOrderRole {
        private String roleType;
        private String userId;

        public DemoDataOrderRole() {
        }

        public DemoDataOrderRole(String roleType, String userId) {
            this.roleType = roleType;
            this.userId = userId;
        }

        public String getRoleType() {
            return roleType;
        }

        public void setRoleType(String roleType) {
            this.roleType = roleType;
        }

        public String getUserId() {
            return userId;
        }

        public void setUserId(String userId) {
            this.userId = userId;
        }
    }

    public class DemoDataOrderStatus {
        private String orderStatusId;
        private String statusId;
        private Timestamp statusDate;

        public DemoDataOrderStatus() {
        }

        public DemoDataOrderStatus(String orderStatusId, String statusId, Timestamp statusDate) {
            this.orderStatusId = orderStatusId;
            this.statusId = statusId;
            this.statusDate = statusDate;
        }

        public String getStatusId() {
            return statusId;
        }

        public void setStatusId(String statusId) {
            this.statusId = statusId;
        }

        public Timestamp getStatusDate() {
            return statusDate;
        }

        public void setStatusDate(Timestamp statusDate) {
            this.statusDate = statusDate;
        }

        public String getOrderStatusId() {
            return orderStatusId;
        }

        public void setOrderStatusId(String orderStatusId) {
            this.orderStatusId = orderStatusId;
        }

    }

}
