G21                                                      ; [ filament / Filament Change ] Set units to millimeters
M83                                                      ; [ filament / Filament Change ] Set extruder to relative mode
G1    E80.0000      F150 X110.0000   Y5.0000  Z20.0000   ; [ filament / Filament Change ] Linear move with extrusion
M300      P500      S880                                 ; [ filament / Filament Change ] Play tone
