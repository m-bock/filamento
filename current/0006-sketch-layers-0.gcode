G21                                                      ; [ Filament Change ] Set units to millimeters
M83                                                      ; [ Filament Change ] Set extruder to relative mode
M300      P500      S880                                 ; [ Filament Change ] Play tone
G1     E0.0000     F9000  X62.5000  Y62.5000   Z0.2000   ; [ sketch / layers / 0 / nextLayer ] Linear move
