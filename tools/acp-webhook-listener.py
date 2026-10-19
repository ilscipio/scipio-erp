#!/usr/bin/env python3
"""Scipio Commerce - local receiver for ACP order webhooks (test tool).

Checks the Merchant-Signature header (t=<unix seconds>,v1=<hex HMAC-SHA256 of "<t>.<body>">) with the shared
secret and appends one JSON line per event to the output file: {"valid": bool, "type": ..., "order": ..., "status": ...}.

Usage: python3 tools/acp-webhook-listener.py <port> <secret> <output file>
Set acp.properties webhook.url=http://localhost:<port>/agentic_checkout/webhooks/order_events and webhook.secret=<secret>.
"""
import hashlib
import hmac
import json
import sys
import time
from http.server import BaseHTTPRequestHandler, HTTPServer

PORT, SECRET, OUT = int(sys.argv[1]), sys.argv[2], sys.argv[3]


def verify(header, body):
    parts = dict(p.split("=", 1) for p in header.split(",") if "=" in p)
    t, v1 = parts.get("t"), parts.get("v1")
    if not t or not v1 or abs(time.time() - int(t)) > 300:
        return False
    mac = hmac.new(SECRET.encode(), (t + ".").encode() + body, hashlib.sha256).hexdigest()
    return hmac.compare_digest(mac, v1)


class Handler(BaseHTTPRequestHandler):
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get("Content-Length", 0)))
        valid = verify(self.headers.get("Merchant-Signature", ""), body)
        event = json.loads(body or b"{}")
        data = event.get("data", {})
        with open(OUT, "a", encoding="utf-8") as f:
            f.write(json.dumps({"valid": valid, "type": event.get("type"), "order": data.get("id"),
                                "session": data.get("checkout_session_id"), "status": data.get("status")}) + "\n")
        self.send_response(200 if valid else 401)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(b'{"received":true}' if valid else b'{"type":"invalid_request","code":"invalid_signature"}')

    def log_message(self, *args):
        pass


HTTPServer(("127.0.0.1", PORT), Handler).serve_forever()
