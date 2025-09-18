G21                                                      ; [ Filament Change ] Set units to millimeters
M83                                                      ; [ Filament Change ] Set extruder to relative mode
G1    E80.0000      F150   X0.0000   Y0.0000  Z20.0000   ; [ Filament Change ] Linear move with extrusion
M300      P500      S880                                 ; [ Filament Change ] Play tone
