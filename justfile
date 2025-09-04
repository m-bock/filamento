set dotenv-load

gen:
    cabal run

ai-sync-modules:
    cursor-agent -p "Make sure every file in the src and app directory is also listed in the cabal file as exposed module." --model "gpt-5"

octo-cancel-print:
    curl -H "X-Api-Key: ${OCTO_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/job" \
         -d '{"command": "cancel"}'

    sleep 2

octo-upload:
    curl -H "X-Api-Key: ${OCTO_API_KEY}" \
         -F "select=true" \
         -F "print=false" \
         -F "file=@out/myprint.gcode" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/files/local"

octo-print:
    curl -H "X-Api-Key: ${OCTO_API_KEY}" \
         -F "select=true" \
         -F "print=true" \
         -F "file=@out/myprint.gcode" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/files/local"

octo-connect-printer:
    curl -H "X-Api-Key: ${OCTO_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/connection" \
         -d '{"command": "connect"}'

    sleep 2
    just octo-status

octo-disconnect-printer:
    curl -H "X-Api-Key: ${OCTO_API_KEY}" \
         -H "Content-Type: application/json" \
         -X POST \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/connection" \
         -d '{"command": "disconnect"}'

octo-status:
    curl --silent -H "X-Api-Key: ${OCTO_API_KEY}" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/connection" \
    | jq '.current.state'

show:
    code out/myprint.gcode

dev1: gen octo-connect-printer octo-print

dev2: gen octo-upload

octo-start:
    nix-shell -p octoprint --run "octoprint serve"

prusa-show:
    nix-shell -p prusa-slicer --run "prusa-slicer out/myprint.gcode"

octo-web:
    chromium http://localhost:5000