package org.ofbiz.entity;

/**
 * Generic entity data integrity exception, representing code-base data integrity checks.
 *
 * <p>NOTE: This differs from {@link GenericModelException} in that this one represents run-/store-time data errors,
 * whereas model errors refer to errors in xml definitions and such.</p>
 *
 * <p>SCIPIO: 3.0.0: Added to represent server-side data integrity checks, similar to {@link EntityConstraintException}.</p>
 */
public class EntityDataIntegrityException extends GenericEntityException {

    public EntityDataIntegrityException() {
    }

    public EntityDataIntegrityException(Throwable nested) {
        super(nested);
    }

    public EntityDataIntegrityException(String str) {
        super(str);
    }

    public EntityDataIntegrityException(String str, Throwable nested) {
        super(str, nested);
    }

}
