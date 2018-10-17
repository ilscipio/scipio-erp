package com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

public class DemoDataTransaction implements AbstractDataObject {

    private String id;
    private String type;
    private String description;
    private Timestamp date;
    private boolean isPosted;
    private Timestamp postedDate;
    private String fiscalType;
    private List<DemoDataTransactionEntry> entries;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Timestamp getDate() {
        return date;
    }

    public void setDate(Timestamp date) {
        this.date = date;
    }

    public boolean isPosted() {
        return isPosted;
    }

    public void setPosted(boolean isPosted) {
        this.isPosted = isPosted;
    }

    public List<DemoDataTransactionEntry> getEntries() {
        return entries;
    }

    public void setEntries(List<DemoDataTransactionEntry> entries) {
        this.entries = entries;
    }

    public void addEntry(DemoDataTransactionEntry transactionEntry) {
        this.entries.add(transactionEntry);
    }

    public Timestamp getPostedDate() {
        return postedDate;
    }

    public void setPostedDate(Timestamp postedDate) {
        this.postedDate = postedDate;
    }

    public String getFiscalType() {
        return fiscalType;
    }

    public void setFiscalType(String fiscalType) {
        this.fiscalType = fiscalType;
    }

    public class DemoDataTransactionEntry {
        private String sequenceId;
        private String type;
        private String description;
        private String party;
        private String roleType;
        private String glAccount;
        private String glAccountType;
        private String orgParty;
        private BigDecimal amount;
        private String debitCreditFlag;
        private String currency;
        private Timestamp dueDate;

        public String getSequenceId() {
            return sequenceId;
        }

        public void setSequenceId(String sequenceId) {
            this.sequenceId = sequenceId;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public String getParty() {
            return party;
        }

        public void setParty(String party) {
            this.party = party;
        }

        public String getRoleType() {
            return roleType;
        }

        public void setRoleType(String roleType) {
            this.roleType = roleType;
        }

        public String getGlAccount() {
            return glAccount;
        }

        public void setGlAccount(String glAccount) {
            this.glAccount = glAccount;
        }

        public String getGlAccountType() {
            return glAccountType;
        }

        public void setGlAccountType(String glAccountType) {
            this.glAccountType = glAccountType;
        }

        public String getOrgParty() {
            return orgParty;
        }

        public void setOrgParty(String orgParty) {
            this.orgParty = orgParty;
        }

        public BigDecimal getAmount() {
            return amount;
        }

        public void setAmount(BigDecimal amount) {
            this.amount = amount;
        }

        public String isDebitCreditFlag() {
            return debitCreditFlag;
        }

        public void setDebitCreditFlag(String debitCreditFlag) {
            this.debitCreditFlag = debitCreditFlag;
        }

        public String getCurrency() {
            return currency;
        }

        public void setCurrency(String currency) {
            this.currency = currency;
        }

        public Timestamp getDueDate() {
            return dueDate;
        }

        public void setDueDate(Timestamp dueDate) {
            this.dueDate = dueDate;
        }
    }

}