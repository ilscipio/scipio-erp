package com.ilscipio.scipio.ce.demoSuite.dataGenerator.impl;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilRandom;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.store.ProductStoreWorker;

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.AbstractDataGenerator;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder.DemoDataOrderItem;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder.DemoDataOrderRole;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataOrder.DemoDataOrderStatus;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataProduct.DemoDataProductPrice;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataTransaction;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataTransaction.DemoDataTransactionEntry;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataWorkEffort;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.party.DemoDataParty;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper;
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.LocalDemoDataHelper;

/**
 * Demo data generator that doesn't rely in any external data provider
 * 
 * @author jsoto
 *
 */
public class LocalDataGenerator extends AbstractDataGenerator {
    private final static String module = LocalDataGenerator.class.getName();

    private final static String LOCAL_DATA_GENERATOR = "local";

    private final LocalDemoDataHelper helper;

    public LocalDataGenerator(AbstractDemoDataHelper helper) {
        super(helper);
        this.helper = (LocalDemoDataHelper) helper;
    }

    /**
     * ORDER
     */
    final List<String> orderTypes = UtilMisc.toList("PURCHASE_ORDER", "SALES_ORDER");
    final List<String> orderStatusTypes = UtilMisc.toList("ORDER_CREATED", "ORDER_COMPLETED");

    final List<String> prodCatalogCategoryTypes = UtilMisc.toList("PCCT_ADMIN_ALLW", "PCCT_BROWSE_ROOT", "PCCT_MOST_POPULAR", "PCCT_OTHER_SEARCH", "PCCT_PROMOTIONS",
            "PCCT_PURCH_ALLW", "PCCT_QUICK_ADD", "PCCT_SEARCH", "PCCT_VIEW_ALLW", "PCCT_WHATS_NEW");

    /**
     * PRODUCT
     */
    // TODO: Gotta figure how to handle these two types "AGGREGATED",
    // "AGGREGATED_CONF"
    final List<String> productTypes = UtilMisc.toList("ASSET_USAGE", "DIGITAL_GOOD", "FINDIG_GOOD", "FINISHED_GOOD", "GOOD", "MARKETING_PKG_PICK", "MARKETING_PKG_AUTO",
            "RAW_MATERIAL", "SERVICE", "SUBASSEMBLY", "WIP");
    final List<String> productPriceTypes = UtilMisc.toList("LIST_PRICE", "PROMO_PRICE", "SPECIAL_PROMO_PRICE", "WHOLESALE_PRICE");
    final List<String> productPricePurposes = UtilMisc.toList("COMPONENT_PRICE", "RECURRING_PRICE", "USAGE_CHARGE");

    /**
     * WORKEFFORT
     */
    // WorkEffortTypeIds
    final List<String> workEffortTypeIds = UtilMisc.toList("ACTIVITY", "ASSET_USAGE", "AVAILABLE", "BUSINESS_TRAVEL", "EVENT", "MEETING", "MILESTONE", "PERSONAL_TIMEOFF", "PHASE",
            "PHASE_TEMPLATE", "PROD_ORDER_HEADER", "PROD_ORDER_TASK", "PROGRAM", "PROJECT", "PROJECT_TEMPLATE", "PUBLIC_HOLIDAY", "PUBLISH_PROPS", "ROU_TASK", "ROUTING",
            "SCRUM_PROJECT", "SCRUM_SPRINT", "SCRUM_TASK", "SCRUM_TASK_ERROR", "SCRUM_TASK_IMPL", "SCRUM_TASK_INST", "SCRUM_TASK_TEST", "TASK", "TASK_TEMPLATE", "TEMPLATE",
            "TRAINING", "WORK_FLOW");

    // workEffortTypeIdsAndStatus
    final Map<String, List<String>> workEffortTypeIdsAndStatus = UtilMisc.toMap("TASK",
            UtilMisc.toList("CAL_DECLINED", "CAL_DELEGATED", "CAL_COMPLETED", "CAL_CANCELLED", "CAL_ACCEPTED"), "PROD_ORDER_TASK",
            UtilMisc.toList("PRUN_CANCELLED", "PRUN_COMPLETED", "PRUN_CLOSED", "PRUN_CREATED", "PRUN_RUNNING", "PRUN_SCHEDULED", "PRUN_DOC_PRINTED"), "EVENT",
            UtilMisc.toList("CAL_DECLINED", "CAL_DELEGATED", "CAL_COMPLETED", "CAL_CANCELLED", "CAL_CONFIRMED", "CAL_TENTATIVE"), "ACTIVITY",
            UtilMisc.toList("CAL_DECLINED", "CAL_DELEGATED", "CAL_COMPLETED", "CAL_CANCELLED", "CAL_NEEDS_ACTION", "CAL_SENT"));

    final List<String> fixedAssetTypes = UtilMisc.toList("EQUIPMENT", "VEHICLE", "GROUP_EQUIPMENT", "PRODUCTION_EQUIPMENT");

    final List<String> workEffortPartyAssignmentStatus = UtilMisc.toList("PRTYASGN_ASSIGNED", "PRTYASGN_OFFERED", "PRTYASGN_UNASSIGNED");
    final List<String> workEffortAssetAssignmentStatus = UtilMisc.toList("FA_ASGN_ASSIGNED", "FA_ASGN_DENIED", "FA_ASGN_REQUESTED");

    /**
     * ACCOUNTING
     */
    // Expense GlAccounts per GlAccountClass
    private final static Map<String, List<String>> glExpenseAccountClassIds = UtilMisc.toMap("EXPENSE", UtilMisc.toList("513000", "513100", "513200", "600000"), "CASH_EXPENSE",
            UtilMisc.toList("820000", "824000", "829000"), "NON_CASH_EXPENSE",
            UtilMisc.toList("515000", "515100", "515200", "516000", "516100", "517000", "517100", "517200", "517300", "517400", "518000", "518100", "518200", "519000", "519100",
                    "519200", "823000"),
            "COGS_EXPENSE",
            UtilMisc.toList("410000", "500000", "501000", "502000", "503000", "510000", "510200", "511000", "511100", "511200", "512000", "512050", "512100", "512200"),
            "INVENTORY_ADJUST", UtilMisc.toList("514000"), "SGA_EXPENSE",
            UtilMisc.toList("601000", "601100", "601200", "601300", "601400", "602000", "602100", "602200", "603000", "603100", "603200", "604000", "604100", "604200", "604500",
                    "604600", "604700", "605000", "605100", "605200", "605800", "605900", "606000", "607000", "607100", "607200", "607400", "607500", "607600", "607700", "607900",
                    "608000", "609000", "609100", "609200", "609500", "610000", "611000", "611100", "611200", "611300", "612000", "612100", "612200", "612300", "612400", "613000",
                    "613100", "613200", "613300", "620000", "621000", "621100", "621200", "621300", "621400", "621500", "622000", "622100", "622200", "623000", "623100", "623200",
                    "624000", "624100", "624200", "624300", "624400", "625000", "626000", "626100", "626200", "626300", "626400", "630000", "631000", "631100", "631200", "631300",
                    "631400", "632000", "640000", "641000", "642000", "643000", "649000", "650000", "651000", "652000", "653000", "654000", "659000", "660000", "661000", "662000",
                    "663000", "669000", "680000", "681000", "682000", "683000", "690000", "691000", "692000", "693000", "694000", "700000", "701000", "701100", "701200", "701300",
                    "701400", "702000", "709000", "710000", "711000", "712000", "712100", "712200", "712300", "720000", "721000", "730000", "731000", "731100", "739000", "740000",
                    "741000", "742000", "743000", "744000", "750000", "751000", "752000", "753000", "754000", "760000", "761000", "762000", "763000", "770000", "771000", "772000",
                    "773000", "774000", "779000", "780000", "781000", "782000", "783000", "784000", "785000", "786000", "788000", "789000", "790000"),
            "DEPRECIATION", UtilMisc.toList("670000", "671000", "672000", "673000", "674000", "675000", "675100", "675200", "675300", "675400"), "AMORTIZATION",
            UtilMisc.toList("787000"), "INTEREST_EXPENSE", UtilMisc.toList("821000", "823000"));

    // AcctgTransTypeId
    private final static List<String> acctgTransTypeIds = UtilMisc.toList("AMORTIZATION", "CAPITALIZATION", "CREDIT_LINE", "CREDIT_MEMO", "DEPRECIATION", "DISBURSEMENT",
            "EXTERNAL_ACCTG_TRANS", "INCOMING_PAYMENT", "INTERNAL_ACCTG_TRANS", "INVENTORY", "INVENTORY_RETURN", "ITEM_VARIANCE", "MANUFACTURING", "NOTE", "OBLIGATION_ACCTG_TRA",
            "OTHER_INTERNAL", "OTHER_OBLIGATION", "OUTGOING_PAYMENT", "PAYMENT_ACCTG_TRANS", "PAYMENT_APPL", "PERIOD_CLOSING", "PURCHASE_INVOICE", "RECEIPT", "SALES",
            "SALES_INVOICE", "SALES_SHIPMENT", "SHIPMENT_RECEIPT", "TAX_DUE");

    // Income GlAccounts per GlAccountClass
    private final static Map<String, List<String>> glIncomeAccountClassIds = UtilMisc.toMap("INCOME", UtilMisc.toList("800000"), "CASH_INCOME",
            UtilMisc.toList("801000", "802000", "803000", "804000", "805000", "806000", "810000", "811000", "812000", "813000", "814000", "819000"));

    @Override
    public List<? extends AbstractDataObject> retrieveData() throws Exception {
        List<AbstractDataObject> results = new ArrayList<AbstractDataObject>();
        for (int i = 0; i < helper.getCount(); i++) {
            if (helper.getReturnObjectClass().equals(DemoDataOrder.class)) {
                results.add(generateOrderData());
            } else if (helper.getReturnObjectClass().equals(DemoDataProduct.class)) {
                results.add(generateProductData());
            } else if (helper.getReturnObjectClass().equals(DemoDataWorkEffort.class)) {
                results.add(generateWorkeffortData());
            } else if (helper.getReturnObjectClass().equals(DemoDataTransaction.class)) {
                results.add(generateTransactionData());
            } else if (helper.getReturnObjectClass().equals(DemoDataParty.class)) {
                throw new UnsupportedOperationException("Party demo data is not supported");
            }
        }
        return results;
    }

    private DemoDataOrder generateOrderData() throws Exception {
        Delegator delegator = helper.getDelegator();
        Map<String, Object> context = helper.getContext();

        DemoDataOrder order = new DemoDataOrder();
        BigDecimal remainingSubTotal = new BigDecimal(0.00);
        BigDecimal grandTotal = new BigDecimal(0.00);

        String orderType = (String) context.get("orderType");

        GenericValue party = null;
        GenericValue userLogin = null;
        if (orderType.equals("SALES_ORDER")) {
            party = (GenericValue) context.get("customerParty");
            userLogin = (GenericValue) context.get("customerUserLogin");
        } else {
            party = (GenericValue) context.get("supplierParty");
            userLogin = (GenericValue) context.get("supplierUserLogin");
        }
        if (UtilValidate.isEmpty(party) || UtilValidate.isEmpty(userLogin)) {
            throw new Exception("Invalid party/userLogin");
        }

        String partyId = party.getString("partyId");
        String userLoginId = userLogin.getString("userLoginId");

        // Create OrderHeader
        order.setOrderId("GEN_" + delegator.getNextSeqId("OrderHeader"));

        // Create OrderItem (between 1 and 3)
        int orderItemCount = UtilRandom.getRandomInt(1, 3);

        EntityFindOptions efo = new EntityFindOptions();
        efo.setMaxRows(1);
        for (int orderItemSeqId = 1; orderItemSeqId <= orderItemCount; orderItemSeqId++) {
            efo.setOffset(UtilRandom.getRandomInt(0, (int) context.get("totalProductCount") - 1));
            List<GenericValue> products = delegator.findList("Product", null, null, null, efo, true);
            if (UtilValidate.isNotEmpty(products)) {
                DemoDataOrderItem item = order.new DemoDataOrderItem();

                GenericValue product = products.get(0);
                List<GenericValue> productPrices = product.getRelated("ProductPrice", null, null, false);
                BigDecimal defaultPrice = BigDecimal.ZERO;
                BigDecimal listPrice = BigDecimal.ZERO;
                for (GenericValue productPrice : productPrices) {
                    if (productPrice.get("productPriceTypeId").equals("DEFAULT_PRICE")) {
                        defaultPrice = productPrice.getBigDecimal("price");
                    } else if (productPrice.get("productPriceTypeId").equals("LIST_PRICE")) {
                        listPrice = productPrice.getBigDecimal("price");
                    }
                }

                item.setOrderItemId(String.valueOf(orderItemSeqId));
                item.setProductId(product.getString("productId"));
                item.setQuantity(new BigDecimal(UtilRandom.getRandomInt(0, 10)));

                item.setSelectedAmount(new BigDecimal(0.0));
                item.setUnitPrice(defaultPrice);
                item.setUnitListPrice(listPrice);
                item.setItemCost(item.getUnitPrice().multiply(item.getQuantity()));

                remainingSubTotal = remainingSubTotal.add(item.getItemCost());
                grandTotal = grandTotal.add(item.getItemCost());

                order.addOrderItem(item);
            }
        }

        order.setOrderDate(UtilRandom.generateRandomTimestamp(context));
        order.setOrderType(orderType);
        TimeZone timeZone = (TimeZone) context.get("timeZone");
        if (UtilValidate.isEmpty(timeZone)) {
            timeZone = TimeZone.getDefault();
        }
        order.setOrderName("Demo Order " + UtilDateTime.timeStampToString(order.getOrderDate(), timeZone, (Locale) context.get("locale")));
        order.setRemainingSubTotal(remainingSubTotal);
        order.setGrandTotal(grandTotal);
        order.setCreatedBy(userLoginId);

        // Create basic roles
        List<DemoDataOrderRole> roles = order.getOrderRoles();
        roles.add(order.new DemoDataOrderRole("INTERNAL_ORGANIZATIO", (String) context.get("partyGroupId")));

        if (orderType.equals("SALES_ORDER")) {
            List<GenericValue> orderSalesChannelList = EntityQuery.use(delegator).from("Enumeration").where(UtilMisc.toMap("enumTypeId", "ORDER_SALES_CHANNEL")).queryList();
            GenericValue orderSalesChannel = orderSalesChannelList.get(UtilRandom.random(orderSalesChannelList));
            order.setChannel(orderSalesChannel.getString("enumId"));

            // Create advanced roles
            roles.add(order.new DemoDataOrderRole("CUSTOMER", partyId));
            roles.add(order.new DemoDataOrderRole("PLACING_CUSTOMER", partyId));
            roles.add(order.new DemoDataOrderRole("SHIP_TO_CUSTOMER", partyId));

            // Create contact mechs
            List<DemoDataOrder.DemoDataOrderContactMech> orderContactMechs = order.getOrderContactMechs();
            GenericValue shippingLocation = EntityQuery.use(delegator).from("PartyContactMechAndPurpose").where("partyId", partyId, "contactMechPurposeTypeId", "SHIPPING_LOCATION").queryFirst();
            if (UtilValidate.isNotEmpty(shippingLocation)) {
                orderContactMechs.add(order.new DemoDataOrderContactMech("SHIPPING_LOCATION", shippingLocation.getString("contactMechId")));
            }
            GenericValue billingLocation = EntityQuery.use(delegator).from("PartyContactMechAndPurpose").where("partyId", partyId, "contactMechPurposeTypeId", "BILLING_LOCATION").queryFirst();
            if (UtilValidate.isNotEmpty(billingLocation)) {
                orderContactMechs.add(order.new DemoDataOrderContactMech("BILLING_LOCATION", billingLocation.getString("contactMechId")));
            }
            GenericValue emailAddress = EntityQuery.use(delegator).from("PartyContactMechAndPurpose").where("partyId", partyId, "contactMechPurposeTypeId", "EMAIL_ADDRESS").queryFirst();
            if (UtilValidate.isNotEmpty(emailAddress)) {
                orderContactMechs.add(order.new DemoDataOrderContactMech("EMAIL_ADDRESS", emailAddress.getString("infoString")));
            }
        } else {
            roles.add(order.new DemoDataOrderRole("SUPPLIER", partyId));
        }

        // Create OrderStatus
        String orderStatusId = "GEN_" + delegator.getNextSeqId("OrderStatus");
        List<DemoDataOrderStatus> statuses = order.getOrderStatuses();
        statuses.add(order.new DemoDataOrderStatus(orderStatusId, "ORDER_CREATED", order.getOrderDate()));
        if (UtilRandom.getRandomBoolean() == true) {
            orderStatusId = "GEN_" + delegator.getNextSeqId("OrderStatus");
            if (UtilRandom.getRandomBoolean() == true) {
                statuses.add(order.new DemoDataOrderStatus(orderStatusId, "ORDER_COMPLETED", order.getOrderDate()));
            } else {
                statuses.add(order.new DemoDataOrderStatus(orderStatusId, "ORDER_CANCELLED", order.getOrderDate()));
            }
        }
        return order;
    }

    private DemoDataProduct generateProductData() {
        Delegator delegator = helper.getDelegator();
        Map<String, Object> context = helper.getContext();

        double maxPrice = UtilProperties.getPropertyAsDouble("demosuite", "demosuite.test.data.product.maxPrice", 99999.99);
        double minPrice = UtilProperties.getPropertyAsDouble("demosuite", "demosuite.test.data.product.minPrice", 0.10);

        DemoDataProduct product = new DemoDataProduct();

        @SuppressWarnings("unchecked")
        List<String> productCategoryIds = (List<String>) context.get("productCategoryIds");

        // Create Product
        product.setId("GEN_" + delegator.getNextSeqId("demo-product"));
        product.setCategory(productCategoryIds.get(UtilRandom.random(productCategoryIds)));
        product.setType(productTypes.get(UtilRandom.random(productTypes)));
        product.setCategoryType(prodCatalogCategoryTypes.get(UtilRandom.random(prodCatalogCategoryTypes)));
        product.setIntroductionDate(UtilRandom.generateRandomTimestamp(context));
        product.setName(product.getId() + "_NAME");

        // Create ProductPrice
        GenericValue productStore = ProductStoreWorker.getProductStore((String) context.get("productStoreId"), delegator);
        String currency = productStore.getString("defaultCurrencyUomId");
        if (UtilValidate.isEmpty(currency)) {
            currency = UtilProperties.getPropertyValue("general", "currency.uom.id.default");
        }
        DemoDataProductPrice defaultPrice = product.new DemoDataProductPrice(UtilRandom.getRandomBigDecimal(minPrice, maxPrice), "DEFAULT_PRICE", "PURCHASE", currency);
        product.addPrice(defaultPrice);
        if (UtilRandom.getRandomInt(0, 9) >= 8) {
            if (UtilRandom.getRandomInt(0, 9) >= 8) {
                product.addPrice(product.new DemoDataProductPrice(UtilRandom.getRandomBigDecimal(minPrice, maxPrice), productPriceTypes.get(UtilRandom.random(productPriceTypes)),
                        productPricePurposes.get(UtilRandom.random(productPricePurposes)), currency));
            } else {
                product.addPrice(product.new DemoDataProductPrice(UtilRandom.getRandomBigDecimal(minPrice, maxPrice), productPriceTypes.get(UtilRandom.random(productPriceTypes)),
                        "PURCHASE", currency));
            }
        }

        return product;
    }

    private DemoDataWorkEffort generateWorkeffortData() {
        Delegator delegator = helper.getDelegator();
        Map<String, Object> context = helper.getContext();
        DemoDataWorkEffort workEffort = new DemoDataWorkEffort();

        // if (Boolean.TRUE.equals((context.get("autoCreatePartyRoles"))) {
        // fields = ["partyId": context.partyId, "roleTypeId" :
        // "INTERNAL_ORGANIZATIO"];
        // GenericValue partyRole = delegator.findOne("PartyRole", fields,
        // false);
        // if (partyRole == null) {
        // partyRole = delegator.makeValue("PartyRole", fields);
        // toBeStored.add(partyRole);
        // }
        // }

        workEffort.setId("GEN_" + delegator.getNextSeqId("demo-workEffortId"));
        List<String> workEffortTypeIdsAndStatusKeys = new ArrayList<String>(workEffortTypeIdsAndStatus.keySet());
        workEffort.setType(workEffortTypeIdsAndStatusKeys.get(UtilRandom.random(workEffortTypeIdsAndStatusKeys)));
        List<String> workEffortTypeIdsAndStatusList = workEffortTypeIdsAndStatus.get(workEffort.getType());
        workEffort.setStatus(workEffortTypeIdsAndStatusList.get(UtilRandom.random(workEffortTypeIdsAndStatusList)));
        workEffort.setName("Demo WorkEffort Name " + workEffort.getId());
        Timestamp minDate = UtilRandom.generateRandomTimestamp(context);
        if (context.get("minDate") != null)
            minDate = (Timestamp) context.get("minDate");
        workEffort.setEstimatedStart(minDate);
        workEffort.setEstimatedCompletion(UtilRandom.generateRandomTimestamp(workEffort.getEstimatedStart(), context));
        workEffort.setActualStart(minDate);
        workEffort.setActualCompletion(UtilRandom.generateRandomTimestamp(workEffort.getActualStart(), context));
        
        workEffort.setCreatedDate(UtilDateTime.nowTimestamp());

        GenericValue fixedAsset = null;
        String fixedAssetTypeId = fixedAssetTypes.get(UtilRandom.random(fixedAssetTypes));        
        try {
            List<GenericValue> fixedAssets = EntityQuery.use(delegator).from("FixedAsset").where(UtilMisc.toMap("fixedAssetTypeId", fixedAssetTypeId)).queryList();
            fixedAsset = fixedAssets.get(UtilRandom.random(fixedAssets));
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
        }  

        if (UtilValidate.isNotEmpty(fixedAsset) && UtilRandom.getRandomBoolean()) {            
            String assetStatusId = workEffortAssetAssignmentStatus.get(UtilRandom.random(workEffortAssetAssignmentStatus));            
            workEffort.setAssetStatus(assetStatusId);
            workEffort.setFixedAsset(fixedAsset.getString("fixedAssetId"));
        } else {
            String partyStatusId = workEffortPartyAssignmentStatus.get(UtilRandom.random(workEffortPartyAssignmentStatus));
            workEffort.setPartyStatus(partyStatusId);
        }
        return workEffort;
    }

    private DemoDataTransaction generateTransactionData() {
        Delegator delegator = helper.getDelegator();
        Map<String, Object> context = helper.getContext();
        DemoDataTransaction transaction = new DemoDataTransaction();

        // Create AcctgTrans
        transaction.setId("GEN_" + delegator.getNextSeqId("demo-acctgTransId"));
        // Create AcctgTransEntry (2,4,6)
        int acctgTransEntryCount = UtilRandom.getRandomEvenInt(2, 6);
        transaction.setEntries(new ArrayList<DemoDataTransactionEntry>(acctgTransEntryCount));
        // Determine if it is an income or an expense
        int incomeExpense = UtilRandom.getRandomInt(1, 2);

        String currencyUomId = UtilProperties.getProperties("general.properties").getProperty("currency.uom.id.default", "USD");
        for (int acctgTransEntrySeqId = 1; acctgTransEntrySeqId <= acctgTransEntryCount; acctgTransEntrySeqId++) {
            DemoDataTransactionEntry transactionEntry = transaction.new DemoDataTransactionEntry();

            transactionEntry.setSequenceId(String.valueOf(acctgTransEntrySeqId));
            transactionEntry.setDebitCreditFlag("C");
            if (acctgTransEntrySeqId % 2 == 0) {
                transactionEntry.setDebitCreditFlag("D");
            }
            List<String> glAccountClassIdList = null;
            if (incomeExpense == 1) {
                List<String> keys = new ArrayList<>(glExpenseAccountClassIds.keySet());
                glAccountClassIdList = glExpenseAccountClassIds.get(keys.get(UtilRandom.random(keys)));
            } else {
                List<String> keys = new ArrayList<>(glIncomeAccountClassIds.keySet());
                glAccountClassIdList = glIncomeAccountClassIds.get(keys.get(UtilRandom.random(keys)));
            }
            transactionEntry.setAmount(new BigDecimal(UtilRandom.getRandomInt(10, 10000)));
            transactionEntry.setGlAccount(glAccountClassIdList.get(UtilRandom.random(glAccountClassIdList)));

            DynamicViewEntity dve = new DynamicViewEntity();
            dve.addMemberEntity("GA", "GlAccount");
            dve.addMemberEntity("GAO", "GlAccountOrganization");
            dve.addAliasAll("GA", "", null); // no prefix
            dve.addAlias("GAO", "organizationPartyId");
            dve.addViewLink("GA", "GAO", Boolean.FALSE, UtilMisc.toList(new ModelKeyMap("glAccountId", "glAccountId")));
            try {
                GenericValue glAccount = EntityQuery.use(delegator).from(dve).where("glAccountId", transactionEntry.getGlAccount()).queryOne();
                if (UtilValidate.isNotEmpty(glAccount)) {
                    transactionEntry.setGlAccountType(glAccount.getString("glAccountTypeId"));
                }
            } catch (GenericEntityException e) {
                Debug.logError(e.getMessage(), module);
            }

            transactionEntry.setCurrency(currencyUomId);
            transaction.addEntry(transactionEntry);
        }

        // FIXME: This may be inconsistent with the GlAccount selected, it may
        // affect the accuracy of some reports
        transaction.setType(acctgTransTypeIds.get(UtilRandom.random(acctgTransTypeIds)));
        transaction.setDescription("Demo Transaction " + transaction.getId());
        transaction.setDate(UtilRandom.generateRandomTimestamp(context));
        transaction.setPosted(true);
        transaction.setPostedDate(UtilRandom.generateRandomTimestamp(transaction.getDate(), context));
        transaction.setFiscalType("ACTUAL");

        return transaction;
    }

    @Override
    public AbstractDataObject handleData(Object result, String format) {
        return null;
    }

    @Override
    public String getDataGeneratorName() {
        return LOCAL_DATA_GENERATOR;
    }

}
