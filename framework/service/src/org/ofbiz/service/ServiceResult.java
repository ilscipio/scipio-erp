package org.ofbiz.service;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A class encapsulating and (eventually) representing service results (OUT parameters).
 *
 * <p>Instances are created using factory methods on {@link ServiceContext} itself via {@link ServiceResultFactory} mixin.
 * You simply need to call {@link ServiceContext#success()}, {@link ServiceContext#error(String)}, {@link ServiceContext#fail(String)}
 * or {@link ServiceContext#responseCopy(Map)} on the service context instance for the service (invocation) for which the result is
 * being returned.</p>
 *
 * <p>SCIPIO: 3.0.0: Added to supplement {@link ServiceContext}.</p>
 */
public class ServiceResult extends ServiceContext {

    protected ServiceResult(ModelService service, DispatchContext dctx, Map<String, ?> context) {
        super(service, dctx, context);
    }

    protected ServiceResult(ServiceResult other, ModelService service, DispatchContext dctx, Map<String, ?> context) {
        super(other, service, dctx, context);
    }

    /**
     * Returns a copy of this ServiceResult with a shallow copy of its original context.
     */
    public ServiceResult copy() {
        return copy(null, null, new HashMap<>(context()));
    }

    protected ServiceResult copy(ModelService service, DispatchContext dctx, Map<String, ?> context) {
        return new ServiceResult(this, service, dctx, context);
    }

    /**
     * Returns a new ServiceResult.
     */
    public static ServiceResult from(ModelService service, DispatchContext dctx, Map<String, ?> context) {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return new ServiceResult(service, dctx, context);
    }

    /**
     * Returns a new ServiceResult.
     */
    public static ServiceResult from(ModelService service, LocalDispatcher dispatcher, Map<String, ?> context) {
        return from(service, dispatcher.getDispatchContext(), context);
    }

    /**
     * Returns a new ServiceResult.
     */
    public static ServiceResult from(String serviceName, DispatchContext dctx, Map<String, ?> context) throws GenericServiceException {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return from(dctx.getModelService(serviceName), dctx, context);
    }

    /**
     * Returns a new ServiceResult.
     */
    public static ServiceResult from(String serviceName, LocalDispatcher dispatcher, Map<String, ?> context) throws GenericServiceException {
        return from(serviceName, dispatcher.getDispatchContext(), context);
    }

    /**
     * Returns a new ServiceResult, using the currently-running service to determine the model service.
     *
     * <p>WARN: not always accurate; prefer {@link #from(ModelService, DispatchContext, Map)} or
     * {@link #from(String, DispatchContext, Map)}.</p>
     */
    public static ServiceResult from(DispatchContext dctx, Map<String, ?> context) {
        // NOTE: ? should always be Object, this is for simplicity/compatibility, could be misused but never happens
        return from(dctx.getModelService(), dctx, context);
    }

    /**
     * Returns a new ServiceResult, using the currently-running service to determine the model service.
     *
     * <p>WARN: not always accurate; prefer {@link #from(ModelService, LocalDispatcher, Map)} or
     * {@link #from(String, LocalDispatcher, Map)}.</p>
     */
    public static ServiceResult from(LocalDispatcher dispatcher, Map<String, ?> context) {
        return from(dispatcher.getDispatchContext(), context);
    }

    /**
     * Return this ServiceResult with a substitute service model and context map, for delegation.
     */
    @Override
    public ServiceResult from(ModelService service, Map<String, ?> context) {
        return (ServiceResult) super.from(service, context);
    }

    /**
     * Return this ServiceResult with a substitute context map, for delegation.
     */
    @Override
    public ServiceResult from(Map<String, ?> context) {
        return from((ModelService) null, context);
    }

    /**
     * Returns true if the service response message in this service result is success.
     */
    public boolean isSuccess() {
        return ServiceUtil.isSuccess(context());
    }

    /**
     * Returns true if the service response message in this service result is error.
     */
    public boolean isError() {
        return ServiceUtil.isError(context());
    }

    /**
     * Returns true if the service response message in this service result is fail.
     */
    public boolean isFail() {
        return ServiceUtil.isFailure(context());
    }

    /**
     * Returns the service result responseMessage field (response code), or "success" if null.
     */
    public String responseCode() {
        return ServiceUtil.getResponse(context());
    }

    /**
     * Returns the value of the successMessage field.
     */
    public String successMessage() {
        return (String) context().get(ModelService.SUCCESS_MESSAGE);
    }

    /**
     * Returns the value of the successMessageList field.
     */
    public List<Object> successMessageList() {
        return UtilGenerics.cast(context().get(ModelService.SUCCESS_MESSAGE_LIST));
    }

    /**
     * Returns the value of the errorMessage field.
     */
    public String errorMessage() {
        return (String) context().get(ModelService.ERROR_MESSAGE);
    }

    /**
     * Returns the value of the errorMessageList field.
     */
    public List<Object> errorMessageList() {
        return UtilGenerics.cast(context().get(ModelService.ERROR_MESSAGE_LIST));
    }

    /**
     * Returns the value of the errorMessageMap field.
     */
    public Map<String, Object> errorMessageMap() {
        return UtilGenerics.cast(context().get(ModelService.ERROR_MESSAGE_MAP));
    }

    /**
     * Returns the success message and list messages concatenated into one text.
     */
    public String successText() {
        return ServiceUtil.getSuccessMessage(context());
    }

    /**
     * Returns the error message and list messages concatenated into one text.
     */
    public String errorText() {
        return ServiceUtil.getErrorMessage(context());
    }

    /**
     * Returns the success and error message and list messages concatenated into one text.
     */
    public String successErrorText() {
        return ServiceUtil.getErrorAndSuccessMessage(context());
    }

}
