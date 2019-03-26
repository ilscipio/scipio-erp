package org.ofbiz.entity;

/**
 * SCIPIO: Now thrown instead of IllegalArgumentException when {@link GenericEntity#get(Object)}
 * is called using a field name that does not exist on the entity.
 * <p>
 * Note this extends IllegalArgumentException, NOT GenericEntityException.
 * <p>
 * Added 2018-09-29.
 */
@SuppressWarnings("serial")
public class EntityFieldNotFoundException extends IllegalArgumentException {
    public EntityFieldNotFoundException() {
        super();
    }

    public EntityFieldNotFoundException(String message, Throwable cause) {
        super(message, cause);
    }

    public EntityFieldNotFoundException(String s) {
        super(s);
    }

    public EntityFieldNotFoundException(Throwable cause) {
        super(cause);
    }
}