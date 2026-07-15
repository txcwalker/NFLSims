# Production Migration & API Security Plan

This document details the strategies and requirements for publishing the NFLSims platform to a production environment, specifically focusing on domain management, API security, and changes we can implement immediately during local development.

---

## 1. Domain & URL Strategy

### In Development (Localhost)
*   **Frontend**: `http://localhost:5173`
*   **FastAPI Backend**: `http://127.0.0.1:8000`

### In Production (Published)
To host the platform publicly, we will map subdomains under a unified root domain (e.g., `nflsims.com`):
*   **Frontend User Dashboard**: `https://nflsims.com` (or `https://www.nflsims.com`)
*   **API Gateway**: `https://api.nflsims.com` (or served as a subpath under `https://nflsims.com/api` via a reverse proxy like Nginx or AWS CloudFront).

---

## 2. API Security Architecture

### A. Strict CORS Policies
In production, we must change from allowing all origins (`*`) to explicitly white-listing the frontend domain:
```python
# Production CORS config
allow_origins = [
    "https://nflsims.com",
    "https://www.nflsims.com"
]
```

### B. HTTPS/SSL Encryption
*   All web traffic will run through secure sockets (HTTPS / WSS) to ensure request payloads (such as user credentials or proprietary optimizer settings) are fully encrypted.
*   We will use **Let's Encrypt** certificates handled by Nginx or Cloudflare.

### C. Rate Limiting (Protection against Resource Exhaustion)
*   Because the `/api/simulate` endpoint executes 10,000 Monte Carlo simulation loops on-demand, it consumes significant CPU power. Malicious actors could easily crash the server by scripting parallel POST requests to this route.
*   **Solution**: Integrate `slowapi` (FastAPI-compatible rate limiter) to limit simulation requests to a maximum of 5 requests/minute per client IP address.

### D. JWT Authentication & Authorization
*   For premium models, DFS exports, and advanced prop-finder dashboards, we will protect endpoints with standard JSON Web Token (JWT) headers. 
*   Requests to premium endpoints will require an `Authorization: Bearer <TOKEN>` header.

---

## 3. What We Can Implement Right Now in Development

We don't have to wait for production deployment to build secure and clean patterns. We are implementing these features in the current phase:

1.  **Dynamic Environment Variables**: 
    *   Configure Vite to pull the API address from a `.env` file via `import.meta.env.VITE_API_URL` instead of hardcoding `127.0.0.1:8000`.
2.  **Explicit Root diagnostic `/` route**: 
    *   Returns the active API status and a clickable link to interactive documentation (`/docs`), resolving the standard `404 Not Found` page when checking the port.
3.  **Dedicated Health Checks (`/health` & `/api/health`)**:
    *   Returns whether the underlying parquet simulator database is correctly loaded in RAM, verifying server readiness.
4.  **CORS Environment Handling**:
    *   Configure FastAPI to load allowed origins from an environment variable, defaulting to `["*"]` locally but ready to adapt in production.
