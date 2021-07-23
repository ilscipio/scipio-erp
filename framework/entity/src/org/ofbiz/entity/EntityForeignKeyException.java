package org.ofbiz.entity;

/**
 * Thrown upon a foreign key database entity constraint violation.
 * <p>SCIPIO: 2.1.0: Added toward specific exception support.</p>
 */
@SuppressWarnings("serial")
public class EntityForeignKeyException extends EntityConstraintException {
    public EntityForeignKeyException(String message, Throwable cause) {
        super(message, cause);
    }
}
