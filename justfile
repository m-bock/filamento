set dotenv-load

gen:
    cabal run

octo-cancel-print:
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/job" \
         -d '{"command": "cancel"}'

    sleep 2

octo-upload:
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -F "select=true" \
         -F "print=false" \
         -F "file=@out/myprint.gcode" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/files/local"

octo-print:
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

dev1: gen octo-cancel-print octo-print

dev2: gen octo-upload

octo-start:
    nix-shell -p octoprint --run "octoprint serve"