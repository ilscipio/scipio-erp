<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#-- SCIPIO: 4.0.0: the card pay step of a hosted store (EXT_STRIPE_HUB, W1-10d).
     The store holds no Stripe key: the browser sends the signed checkout token to the hub, the hub creates the
     PaymentIntent on the connected account of the store and answers with the client secret, the account and the
     publishable key of the platform. Stripe.js shows the Payment Element (card, Apple Pay, Google Pay). The webhook of
     Stripe reaches the hub; the desk records the payment in this store (order/order payment_hub_record). -->
<#assign hubPay = Static["com.ilscipio.scipio.order.payment.HubCheckout"].payStep(request, orderHeader!)>
<#if hubPay.state == "due">
  <@section title=uiLabelMap.OrderHubPayTitle containerId="scp-hubpay-section">
    <div id="scp-hubpay" data-intent-url="${escapeVal(hubPay.intentUrl, 'html')}" data-token="${escapeVal(hubPay.token, 'html')}"
        data-return-url="${escapeVal(hubPay.returnUrl, 'html')}">
      <p>${uiLabelMap.OrderHubPayDesc}</p>
      <div id="scp-hubpay-element"></div>
      <p id="scp-hubpay-message" role="alert" aria-live="polite"></p>
      <button type="button" id="scp-hubpay-submit" class="${styles.link_run_sys!} ${styles.action_update!}" disabled="disabled">${uiLabelMap.OrderHubPayButton} <@ofbizCurrency amount=hubPay.amount isoCode=hubPay.currency/></button>
    </div>
  </@section>
  <script src="https://js.stripe.com/v3/"></script>
  <@script>
    (function() {
        var box = document.getElementById('scp-hubpay');
        var msg = document.getElementById('scp-hubpay-message');
        var btn = document.getElementById('scp-hubpay-submit');
        if (!box || !msg || !btn) { return; }
        if (!window.Stripe) { msg.textContent = '${escapeVal(uiLabelMap.OrderHubPayUnavailable, 'js')}'; return; }
        var query = new URLSearchParams(window.location.search);
        if (query.get('redirect_status') === 'failed') { msg.textContent = '${escapeVal(uiLabelMap.OrderHubPayFailed, 'js')}'; }
        // text/plain keeps the call a simple CORS request (no preflight); the hub answers with the origin of the token only
        fetch(box.getAttribute('data-intent-url'), { method: 'POST', headers: { 'Content-Type': 'text/plain' },
                body: box.getAttribute('data-token'), credentials: 'omit' })
            .then(function(r) { return r.json().then(function(j) { if (!r.ok) { throw new Error(j.message || ('HTTP ' + r.status)); } return j; }); })
            .then(function(j) {
                var stripe = window.Stripe(j.publishableKey, { stripeAccount: j.stripeAccount });
                var elements = stripe.elements({ clientSecret: j.clientSecret });
                elements.create('payment').mount('#scp-hubpay-element');
                btn.disabled = false;
                btn.addEventListener('click', function() {
                    btn.disabled = true;
                    msg.textContent = '';
                    stripe.confirmPayment({ elements: elements, confirmParams: { return_url: box.getAttribute('data-return-url') } })
                        .then(function(res) { if (res.error) { msg.textContent = res.error.message; btn.disabled = false; } });
                });
            })
            .catch(function(e) { msg.textContent = '${escapeVal(uiLabelMap.OrderHubPayUnavailable, 'js')} ' + e.message; });
    })();
  </@script>
<#elseif hubPay.state == "confirming">
  <@alert type="info">${uiLabelMap.OrderHubPayConfirming}</@alert>
<#elseif hubPay.state == "paid">
  <@alert type="success">${uiLabelMap.OrderHubPayPaid}</@alert>
<#elseif hubPay.state == "not_configured" || hubPay.state == "error">
  <@alert type="warning">${uiLabelMap.OrderHubPayUnavailable}</@alert>
</#if>
