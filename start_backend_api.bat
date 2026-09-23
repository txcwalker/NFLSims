@echo off
echo Starting NFLSims: FastAPI Backend API Server on Port 8002...
cd /d "%~dp0"
venv\Scripts\python.exe -m uvicorn src.api.app:app --reload --reload-dir src --port 8002
pause
