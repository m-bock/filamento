# Justfile for jee-code

# Usage: just gen     # generate G-code
#        just print   # upload and print via OctoPrint
# Requires .env file with OCTOPRINT_API_KEY (and optionally OCTOPRINT_URL)

set dotenv-load

gen:
    cabal run

cancel-print:
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/job" \
         -d '{"command": "cancel"}'

    sleep 2

print:
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -F "select=true" \
         -F "print=true" \
         -F "file=@out/myprint.gcode" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/files/local"

octo-connect-printer:
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/connection" \
         -d '{"command": "connect"}'

octo-disconnect-printer:
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/connection" \
         -d '{"command": "disconnect"}'

octo-status:
    curl --silent -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/connection" \
    | jq '.current.state'

show:
    code out/myprint.gcode

dev1: gen cancel-print print show

start-octo:
    nix-shell -p octoprint --run "octoprint serve"