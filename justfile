# Justfile for jee-code

# Usage: just gen     # generate G-code
#        just print   # upload and print via OctoPrint
# Requires .env file with OCTOPRINT_API_KEY (and optionally OCTOPRINT_URL)

set dotenv-load

gen:
    cabal run jee-code-exe > out/myprint.gcode

print: gen
    curl -H "X-Api-Key: ${OCTOPRINT_API_KEY}" \
         -F "select=true" \
         -F "print=true" \
         -F "file=@myprint.gcode" \
         "${OCTOPRINT_URL:-http://localhost:5000}/api/files/local"